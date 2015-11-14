
{-# LANGUAGE OverloadedStrings #-}

module GenC where
import Ast
import qualified Exceptions
import qualified Data.Map as M
import Control.Exception
import TypeCheck
import Data.Maybe

-- for random string generation
import System.Random
import System.IO.Unsafe

gen :: Env Block -> [Block] -> String
gen gamma bs =
         "#ifndef " ++ headerGaurd ++ "\n#define " ++ headerGaurd
      ++ "\n#include <string.h>\n#include <stdint.h>\n#include <endian.h>\n"
      ++ "#include <stdio.h>\n#include <stdbool.h>\n\n"
      ++ concatMap (genBlock gamma) bs
      ++ "\n#endif\n"
          where headerGaurd = "BYTE_BLOCKS__" ++
                    take 20 (randomRs ('A','Z') $ unsafePerformIO newStdGen)

genBlock :: Env Block -> Block -> String
genBlock gamma b = genStructure gamma b ++ ('\n' : '\n' : genRead gamma b)
            ++ ('\n' : '\n' : genWrite gamma b) ++ "\n\n\n"

genStructure :: Env Block -> Block -> String
genStructure gamma (Block n es) =
  "typedef struct " ++ n ++ " {\n"
    ++ concatMap fieldStr es
    ++ "} " ++ n ++ ";\n"
    where
      cFieldDecl cType fName = "    " ++ cType ++ (' ' : fName) ++ ";\n"
      fieldStr (Blk b) = throw $ Exceptions.Unsupported "Nested blocks."
      fieldStr (Field name ty) =
        case ty of
          (BField i s e) ->
            let sign = if s == Signed then "" else "u"
            in if i `elem` [8, 16, 32, 64]
               then cFieldDecl (sign ++ "int" ++ show i ++ "_t") name
               else throw $ Exceptions.Unsupported "Unaligned bitfields."
          -- based on the fact that previously defined structs will already
          -- have been defined in the output file, we can safely use them
          -- as C types.
          (Tycon n) -> cFieldDecl ("struct " ++ n) name
          (TyConapp t tys) ->
            throw $ Exceptions.Unsupported "Higher Order types."

genWrite :: Env Block -> Block -> String
genWrite gamma (Block n entries) =
     "int " ++ n ++ "_write(" ++ n ++ " *src, FILE *f)\n"
  ++ "{\n    uint8_t buff[" ++ bs ++ "];\n\n"
  ++  writeFileStrs entries 0
  ++ "\n    if (fwrite(buff, " ++ bs ++ ", 1, f) != 1) return false;\n"
  ++ "\n    return true;\n}"
    where 
      bs = show $ blockSize gamma (Block n entries)
      writeFileStrs [] _ = ""
      writeFileStrs (e:es) bytesWritten =
        case e of
          (Blk _) -> throw $ Exceptions.Unsupported "Nested blocks."
          (Field fName (BField i s endianness)) ->
            if i `elem` [8, 16, 32, 64]
              then "    *(" ++ wordPtrStr ++ "(buff + " ++ show bytesWritten
                  ++ "))" ++ " = " ++ endianFuncStr
                  ++ "(src->" ++ fName ++ ");\n"
                  ++ writeFileStrs es (bytesWritten + (i `div` 8))
              else throw $ Exceptions.Unsupported "Unaligned bitfields."
                where
                  wordSizeStr = show i
                  signStr = case s of
                              Signed -> ""
                              Unsigned -> "u"
                  endianFuncStr = 
                    if i <= 8
                       then ""
                       else case endianness of
                              BigEndian -> "htobe" ++ wordSizeStr
                              LittleEndian -> "htole" ++ wordSizeStr
                              NativeEndian -> ""
                  wordPtrStr = '(' : signStr ++ "int" ++ wordSizeStr ++ "_t*)"

          (Field fName (Tycon tyName)) ->
            "    " ++ tyName ++ "_write(&(src->" ++ fName ++ "), f);\n"
          (Field name (TyConapp _ _)) ->
            throw $ Exceptions.Unsupported "Higher Order types."


genRead :: Env Block -> Block -> String
genRead gamma (Block n entries) = 
     "int " ++ n ++ "_read(" ++ n ++ " *tgt, FILE *f)\n"
  ++ "{\n    uint8_t buff[" ++ bs ++ "];\n"
  ++ "    memset(buff, 0, " ++ bs ++ ");\n\n"
  ++ "    if (fread(buff, " ++ bs ++ ", 1, f) != 1) return false;\n\n"
  ++ readFieldStrs entries 0
  ++ "\n    return true;\n}"
    where
      bs = show $ blockSize gamma (Block n entries)
      readFieldStrs [] _ = ""
      readFieldStrs (e:es) bytesConsumed =
        case e of
          (Blk _) -> throw $ Exceptions.Unsupported "Nested blocks."
          (Field fName (BField i s endianness)) ->
            if i `elem` [8, 16, 32, 64]
              then "    tgt->" ++ fName
                      ++ " = " ++ endianFuncStr ++ "(* (" ++ wordPtrStr
                      ++ "(buff + " ++ show bytesConsumed ++ ")));\n"
                      ++ readFieldStrs es (bytesConsumed + (i `div` 8))
              else throw $ Exceptions.Unsupported "Unaligned bitfields."
                where
                  wordSizeStr = show i
                  signStr = case s of
                              Signed -> ""
                              Unsigned -> "u"
                  endianFuncStr = 
                    if i <= 8
                       then ""
                       else case endianness of
                              BigEndian -> "be" ++ wordSizeStr ++ "toh"
                              LittleEndian -> "le" ++ wordSizeStr ++ "toh"
                              NativeEndian -> ""
                  wordPtrStr = '(' : signStr ++ "int" ++ wordSizeStr ++ "_t*)"

          (Field fName (Tycon tyName)) ->
            "    " ++ tyName ++ "_read(&(tgt->" ++ fName ++ "), f);\n"
          (Field name (TyConapp _ _)) ->
            throw $ Exceptions.Unsupported "Higher Order types."

blockSize :: Env Block -> Block -> Int
blockSize gamma (Block _ entries) =
      foldl (\a e ->
        case e of
          (Field _ (BField i _ _)) -> 
            if i `elem` [8, 16, 32, 64]
            then a + (i `div` 8)
            else throw $ Exceptions.Unsupported "Unaligned bitfields."
          (Field _ (Tycon tyName)) ->
            blockSize gamma (fromJust (tyName `M.lookup` gamma))
          (Field name (TyConapp t tys)) -> -- TODO cache results instead of recomputing
            throw $ Exceptions.Unsupported "Higher Order Types."
          (Blk _) -> throw $ Exceptions.Unsupported "Nested blocks.")
      0
      entries
