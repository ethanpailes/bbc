
{-# LANGUAGE OverloadedStrings #-}

module GenC where
import Ast
import qualified Exceptions
import qualified Data.Map as M
import Control.Exception
import Control.Monad
import TypeCheck
import Data.Maybe
import Data.Either

-- for random string generation
import System.Random
import System.IO.Unsafe

gen :: Env Block -> [Block] -> String
gen gamma bs =
         "#ifndef " ++ headerGaurd ++ "\n#define " ++ headerGaurd
      ++ "\n#include <string.h>\n#include <stdint.h>\n"
      -- DEPENDENCY WRANGLING
      ++ "#ifdef __linux__\n"
      ++ "#include <endian.h>\n"
      ++ "#elif defined __APPLE__\n"
      ++ "#include <machine/endian.h>\n"
      ++ "#endif\n"
      -- END DEPENDENCY WRANGLING
      ++ "#include <stdio.h>\n\n#define true 1\n"
      ++ "#define false 0\n\n"
      ++ concatMap (genBlock gamma) bs
      ++ "\n#endif\n"
          where headerGaurd = "BYTE_BLOCKS__" ++
                    take 20 (randomRs ('A','Z') $ unsafePerformIO newStdGen)

genBlock :: Env Block -> Block -> String
genBlock gamma b = unlines $ map (\genFun -> genFun gamma b) genFuns
    where
      genFuns = [ genStructure
                , genPack
                , genUnpack
                , \_ _ -> "\n"
                , genWrite
                , genRead
                , \_ _ -> "\n\n"]


genStructure :: Env Block -> Block -> String
genStructure gamma (Block n es) =
  "typedef struct " ++ n ++ " {\n"
    ++ concatMap fieldStr es
    ++ "} " ++ n ++ ";\n"
    where
      cFieldDecl cType fName = "    " ++ cType ++ (' ' : fName) ++ ";\n"


      -- only works on bfields
      cTypeOf (BField i s _) = let sign = if s == Signed then "" else "u"
                                in sign ++ "int" ++ show i ++ "_t"
      cTypeOf _              = "" -- unsafe

      fieldStr (Blk b) = throw $ Exceptions.Unsupported "Nested blocks."
      fieldStr (Field name ty) =
        case ty of
          (BField i s e) ->
            let sign = if s == Signed then "" else "u"
            in if i `elem` [8, 16, 32, 64]
               then cFieldDecl (cTypeOf ty) name
               else throw $ Exceptions.Unsupported "Unaligned bitfields."
          -- based on the fact that previously defined structs will already
          -- have been defined in the output file, we can safely use them
          -- as C types.
          (Tycon n) -> cFieldDecl n name
          (TyConapp t tys) ->
            case t of
              (Tycon "array") ->
                let [tag, content] = tys
                 in fieldStr (Field (name ++ "_len") tag)
                  ++ cFieldDecl (cTypeOf content ++ " *") name

-- genSize gamma (Block blkName entries) =


genWrite :: Env Block -> Block -> String
genWrite gamma (Block n es) =
     "int " ++ n ++ "_write(const " ++ n ++ " *src, FILE *f)\n"
  ++ "{\n"
  ++ case blkSize of
       (Right bs) -> "    size_t blk_size = " ++ show bs ++ ";\n"
                 ++ "    char buff[" ++ show bs ++ "];\n"
       (Left _) -> "    size_t blk_size = " ++ n ++ "_size(src);\n"
               ++ "    char * buff = (char*) malloc(blk_size);\n"
  ++ "    if(!"  ++ n ++ "_pack(src, buff)) return false;\n"
  ++ "    fwrite(buff, blk_size, 1, f);\n}"
  ++ if isLeft blkSize then "    free(buff);\n" else ""
    where blkSize = blockSize gamma (Block n es)

genPack :: Env Block -> Block -> String
genPack gamma (Block n entries) =
     "int " ++ n ++ "_pack(const " ++ n ++ " *src, char *tgt)\n{\n"
  ++  packStmt entries 0
  ++ "\n    return true;\n}"
    where 
      packStmt [] _ = ""
      packStmt (e:es) bytesWritten =
        case e of
          (Blk _) -> throw $ Exceptions.Unsupported "Nested blocks."
          (Field fName (BField i s endianness)) ->
            if i `elem` [8, 16, 32, 64]
              then "    *(" ++ wordPtrStr ++ "(tgt + " ++ show bytesWritten
                  ++ "))" ++ " = " ++ endianFuncStr
                  ++ "(src->" ++ fName ++ ");\n"
                  ++ packStmt es (bytesWritten + (i `div` 8))
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
               "    " ++ tyName ++ "_pack(&(src->"
            ++ fName ++ "), (tgt + " ++ show bytesWritten ++ "));\n"
          (Field name tca@(TyConapp ty tys)) ->
            case ty of
              (Tycon "array") -> "TODO array pack code."
              {-
                let [tag, content] = tys
                 in 
                 -}
              _ -> throw Exceptions.TypeError

genRead :: Env Block -> Block -> String
genRead gamma (Block n entries) = 
     "int " ++ n ++ "_read(" ++ n ++ " *tgt, FILE *f)\n"
  ++ "{\n"
  ++ case blkSize of
       (Right bs) -> "    size_t blk_size = " ++ show bs ++ ";\n"
                 ++ "    char buff[blk_size];\n"
       (Left _)   -> throw $ Exceptions.Unsupported "Variable Length blocks."
  ++ "    if (fread(buff, blk_size, 1, f) != 1) return false;\n"
  ++ "    return " ++ n ++ "_unpack(tgt, buff);\n}"
    where
      blkSize = blockSize gamma (Block n entries)

genUnpack :: Env Block -> Block -> String
genUnpack gamma (Block n entries) = 
     "int " ++ n ++ "_unpack(" ++ n ++ " *tgt, const char *src)\n{\n"
  ++ unpackStmt entries 0
  ++ "\n    return true;\n}"
    where
      unpackStmt [] _ = ""
      unpackStmt (e:es) bytesConsumed =
        case e of
          (Blk _) -> throw $ Exceptions.Unsupported "Nested blocks."
          (Field fName (BField i s endianness)) ->
            if i `elem` [8, 16, 32, 64]
              then "    tgt->" ++ fName
                      ++ " = " ++ endianFuncStr ++ "(* (" ++ wordPtrStr
                      ++ "(src + " ++ show bytesConsumed ++ ")));\n"
                      ++ unpackStmt es (bytesConsumed + (i `div` 8))
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
            "    " ++ tyName ++ "_unpack(&(tgt->"
                ++ fName ++ "), (src + " ++ show bytesConsumed ++ "));\n"
          (Field name (TyConapp _ _)) ->
            "TODO HIGHER ORDER TYPES\n"
            --throw $ Exceptions.Unsupported "Higher Order types."


-- Tries to compute the static size of the block. If the block is variable
-- length (contains an array or list type) returns Left of the static size
-- of the block.
blockSize :: Env Block -> Block -> Either Int Int
blockSize gamma (Block _ entries) =
  let sizeOfType (Field _ (BField i _ _)) =
            if i `elem` [8, 16, 32, 64]
            then Right (i `div` 8)
            else throw $ Exceptions.Unsupported "Unaligned bitfields."
      sizeOfType (Field _ (Tycon tyName)) =
            blockSize gamma (fromJust (tyName `M.lookup` gamma))
      sizeOfType (Field _ tca@(TyConapp t tys)) =
            case t of
              (Tycon "array") ->
                let [tag, _] = tys
                 in case sizeOfType (Field "" tag) of
                      (Right x) -> Left x
                      (Left _) -> throw Exceptions.TypeError -- not possible
              _               -> throw Exceptions.TypeError
      sizeOfType (Blk _) = throw $ Exceptions.Unsupported "Nested blocks."
      
      acc (Right a) (Right x) = Right (a + x)
      acc (Left a) (Right x) = Left (a + x)
      acc (Right a) (Left x) = Left (a + x)
      acc (Left a) (Left x) = Left (a + x)
   in foldl acc (Right 0) $ map sizeOfType entries
