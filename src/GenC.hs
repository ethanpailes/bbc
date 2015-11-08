
{-# LANGUAGE OverloadedStrings #-}

module GenC where
import Ast
import qualified Exceptions
import Control.Exception

-- for random string generation
import System.Random
import System.IO.Unsafe

gen :: [Block] -> String
gen bs = "#ifndef " ++ headerGaurd ++ "\n#define " ++ headerGaurd
      ++ "\n#include <string.h>\n#include <stdint.h>\n#include <endian.h>\n"
      ++ "#include <stdio.h>\n#include <stdbool.h>\n\n"
      ++ concatMap genBlock bs
      ++ "\n#endif\n"
          where headerGaurd = "BYTE_BLOCKS__" ++
                    take 20 (randomRs ('A','Z') $ unsafePerformIO newStdGen)

genBlock :: Block -> String
genBlock b = genStructure b ++ ('\n' : '\n' : genRead b)
            ++ ('\n' : '\n' : genWrite b)

genStructure :: Block -> String
genStructure (Block n es) =
  "typedef struct " ++ n ++ " {\n"
    ++ concatMap fieldStr es
    ++ "} " ++ n ++ ";"
      where fieldStr (Blk b) = throw $ Exceptions.Unsupported "Nested blocks."
            fieldStr (Field name ty) =
              case ty of
                (BField i s e) ->
                  let sign = if s == Signed then "" else "u"
                  in if i `elem` [8, 16, 32, 64]
                     then "    " ++ sign ++ "int" ++ show i ++ "_t "
                            ++ (' ' : name) ++ ";\n"
                     else throw $ Exceptions.Unsupported "Unaligned bitfields."
                (Tycon n) -> throw $ Exceptions.Unsupported "Named types."
                (TyConapp t tys) ->
                  throw $ Exceptions.Unsupported "Higher Order types."

genWrite :: Block -> String
genWrite (Block n entries) =
     "int " ++ n ++ "_write(" ++ n ++ " *src, FILE *f)\n"
  ++ "{\n    uint8_t buff[" ++ bs ++ "];\n\n"
  ++  writeFileStrs entries 0
  ++ "\n    if (fwrite(buff, " ++ bs ++ ", 1, f) != 1) return false;\n"
  ++ "\n    return true;\n}"
    where 
      bs = blockSize entries
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

          (Field name (Tycon _)) ->
            throw $ Exceptions.Unsupported "Named types."
          (Field name (TyConapp _ _)) ->
            throw $ Exceptions.Unsupported "Higher Order types."


genRead :: Block -> String
genRead (Block n entries) = 
     "int " ++ n ++ "_read(" ++ n ++ " *tgt, FILE *f)\n"
  ++ "{\n    uint8_t buff[" ++ bs ++ "];\n"
  ++ "    memset(buff, 0, " ++ bs ++ ");\n\n"
  ++ "    if (fread(buff, " ++ bs ++ ", 1, f) != 1) return false;\n\n"
  ++ readFieldStrs entries 0
  ++ "\n    return true;\n}"
    where
      bs = blockSize entries
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

          (Field name (Tycon _)) ->
            throw $ Exceptions.Unsupported "Named types."
          (Field name (TyConapp _ _)) ->
            throw $ Exceptions.Unsupported "Higher Order types."

blockSize :: [Entry] -> String
blockSize entries = show $
      foldl (\a e ->
        case e of
          (Field name (BField i _ _)) -> 
            if i `elem` [8, 16, 32, 64]
            then a + (i `div` 8)
            else throw $ Exceptions.Unsupported "Unaligned bitfields."
          (Field name (Tycon _)) ->
            throw $ Exceptions.Unsupported "Named types."
          (Field name (TyConapp t tys)) ->
            throw $ Exceptions.Unsupported "Higher Order Types."
          (Blk _) -> throw $ Exceptions.Unsupported "Nested blocks.")
      0
      entries
