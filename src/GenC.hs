
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


-- TODO make this a compiler flag.
readInitialBufferSize :: Int
readInitialBufferSize = 1024

--------------------------------------------------------------------------------


gen :: Env Block -> [Block] -> String
gen gamma bs =
         "#ifndef " ++ headerGaurd ++ "\n#define " ++ headerGaurd
      ++ "\n#include <string.h>\n"
      ++ "#include <stdint.h>\n"
      ++ "#include <endian.h>\n"
      ++ "#include <stdlib.h>\n"
      ++ "#include <stdio.h>\n\n"
      ++ "#define true 1\n"
      ++ "#define false 0\n\n"
      ++ "int grow_buff(char ** buff, size_t * len)\n"
      ++ "{\n"
      ++ "    size_t old_len = *len;\n"
      ++ "    char *tmp = *buff;\n"
      ++ "    *len *=  2;\n"
      ++ "    *buff = malloc(*len);\n"
      ++ "    memcpy(*buff, tmp, old_len);\n"
      ++ "    free(tmp);\n"
      ++ "}\n"
      ++ concatMap (genBlock gamma) bs
      ++ "\n#endif\n"
          where headerGaurd = "BYTE_BLOCKS__" ++
                    take 20 (randomRs ('A','Z') $ unsafePerformIO newStdGen)

genBlock :: Env Block -> Block -> String
genBlock gamma b = unlines $ map (\genFun -> genFun gamma b) genFuns
    where
      genFuns = [ genStructure
                , genSize
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

genSize gamma blk@(Block blkName entries) =
     "inline int " ++ blkName ++ "_size(const " ++ blkName ++ " const * b)\n"
  ++ "{\n"
  ++ "    return " ++ show staticSize
  ++ if isStatic then ";\n}"
                 else concatMap dynamicSizeOf entries ++ ";\n}"
    where blkSize = blockSize gamma blk
          isStatic = isRight blkSize
          staticSize = case blkSize of
                         (Right s) -> s
                         (Left s) -> s
          dynamicSizeOf (Blk _) = throw $ Exceptions.Unsupported "Nested blocks"
          dynamicSizeOf (Field _ (BField {})) = ""
          dynamicSizeOf (Field fName (Tycon tyName)) = 
            -- We know from the typechecker that this type must be in gamma
            " + " ++ tyName ++ "_size(b->" ++ fName ++ ")"
          dynamicSizeOf (Field fName hot@(TyConapp ty tys)) =
            case ty of
              (Tycon "array") -> 
                let [tag, content] = tys
                    bSize = byteSizeOf gamma content
                 in case bSize of
                      (Just i) -> " + (b->" ++fName++ "_len * " ++ show i ++ ")"
                      Nothing ->
                        case content of
                          (Tycon tyName) -> " + " ++ tyName
                                            ++ "_size(&(b->" ++ fName ++ "))"
                          _ -> throw Exceptions.TypeError
              _ -> throw $ Exceptions.MalformedHigherOrderType hot


-- Some blocks it is nice to have lying around for REPL testing
b1 = Block "test1"
        [Field "f1" (BField 16 Signed BigEndian),
         Field "f2" (TyConapp (Tycon "array") [BField 16 Unsigned BigEndian,
                                               BField 32 Unsigned BigEndian])]
b2 = Block "test2"
        [Field "f1" (BField 16 Signed BigEndian)]


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
  ++ "    fwrite(buff, blk_size, 1, f);\n"
  ++ if isLeft blkSize then "    free(buff);\n}" else "}"
    where blkSize = blockSize gamma (Block n es)

genPack :: Env Block -> Block -> String
genPack gamma (Block n entries) =
     "int " ++ n ++ "_pack(const " ++ n ++ " *src, char *tgt)\n{\n"
  ++ "    size_t bytes_written = 0;\n"
  ++  packStmts entries
  ++ "\n    return bytes_written;\n}"
    where 
      packStmt (Blk _) = throw Exceptions.TypeError
      packStmt (Field fName ty@(BField i s endianness)) =
            if i `elem` [8, 16, 32, 64]
              then "    *(" ++ wordPtrStr ++ "(tgt + bytes_written)) = "
                  ++ endianFuncStr ++ "(src->" ++ fName
                  ++ "); bytes_written += " ++
                                show (fromJust (byteSizeOf gamma ty))
                  ++ ";\n"
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

      packStmt (Field fName (Tycon tyName)) =
               "    bytes_written += " ++ tyName ++ "_pack(&(src->"
            ++ fName ++ "), (tgt + bytes_written));\n"
      packStmt (Field fName tca@(TyConapp ty tys)) =
            case ty of
              (Tycon "array") ->
                  let [tag, content] = tys
                      boundCType = cTypeOf tag
                      iteratorName = fName ++ "_iter"
                   in "    " ++ boundCType ++ " " ++ iteratorName ++ ";\n"
                   ++ "    for(" ++ iteratorName ++ " = 0; "
                                ++ iteratorName ++ " < " ++ fName ++ "_len; ++" 
                                ++ iteratorName ++ ") {\n"
                   ++ "    "
                      ++ packStmt (Field (fName ++ "[" ++ iteratorName ++ "]")
                                         content)
                   ++ "    }\n"
              _ -> throw Exceptions.TypeError

      packStmts [] = ""
      packStmts (e:es) =
        case e of
          (Blk _) -> throw $ Exceptions.Unsupported "Nested blocks."
          field -> packStmt field ++ packStmts es


{-
 - The generated code for read has to use a growing buffer to figure out
 - to read in a given byte block.
 -}
genRead :: Env Block -> Block -> String
genRead gamma blk@(Block blkName entries) = 
     "int " ++ blkName ++ "_read(" ++ blkName ++ " *tgt, FILE *f)\n"
  ++ "{\n"
  ++ case blockSize gamma blk of
       (Right bs) -> "    size_t blk_size = " ++ show bs ++ ";\n"
                 ++ "    char buff[blk_size];\n"
       (Left _)   -> "TODO var blk len\n"
  ++ "    if (fread(buff, blk_size, 1, f) != 1) return false;\n"
  ++ "    return " ++ blkName ++ "_unpack(tgt, buff);\n}"

genUnpack :: Env Block -> Block -> String
genUnpack gamma (Block n entries) = 
     "int " ++ n ++ "_unpack(" ++ n ++ " *tgt, const char *src)\n{\n"
  ++ "    size_t bytes_consumed = 0;\n"
  ++ unpackStmts entries
  ++ "\n    return true;\n}"
    where
      unpackStmt (Blk _) = throw $ Exceptions.Unsupported "Nested blocks."
      unpackStmt (Field fName ty@(BField i s endianness)) =
        let wordSizeStr = show i
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
         in if i `elem` [8, 16, 32, 64]
              then "    tgt->" ++ fName
                  ++ " = " ++ endianFuncStr ++ "(* (" ++ wordPtrStr
                  ++ "(src + bytes_consumed))); "
                  ++ "bytes_consumed += "
                  ++ (show (fromJust (byteSizeOf gamma ty)))
                  ++ ";\n"
              else throw $ Exceptions.Unsupported "Unaligned bitfields."

      unpackStmt (Field fName (Tycon tyName)) =
            "    " ++ tyName ++ "_unpack(&(tgt->"
                ++ fName ++ "), (src + bytes_consumed));\n"
      unpackStmt (Field fName (TyConapp ty tys)) =
        case ty of
          (Tycon "array") ->
            let [tag, content] = tys
                iteratorName = fName ++ "_iter"
             in "    " ++ cTypeOf tag ++  ' ' : iteratorName ++ " = 0;\n"
             ++ unpackStmt (Field (fName ++ "_len") tag)
             ++ "    for(" ++ iteratorName ++ " = 0; " ++ iteratorName ++ " < "
                        ++ fName ++ "_len; ++" ++ iteratorName ++ ") {\n"
             ++ "    " ++ unpackStmt
                      (Field (fName ++ '[' : iteratorName ++ "]") content)
             ++ "    }\n"
          _ -> throw Exceptions.TypeError
      unpackStmts [] = ""
      unpackStmts (e:es) =
        case e of
          (Blk _) -> throw $ Exceptions.Unsupported "Nested Blocks"
          field -> unpackStmt field ++ unpackStmts es


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

byteSizeOf :: Env Block -> Ty -> Maybe Int
byteSizeOf _     (BField i _ _) = Just $ i `div` 8
byteSizeOf gamma (Tycon tyName) =
      foldl
        (liftM2 (+))
        (Just 0)
        (map (byteSizeOf gamma . typeOf) entries)
    where (Block _ entries) = fromJust (tyName `M.lookup` gamma)
          typeOf (Blk _) = throw $ Exceptions.Unsupported "Nested blocks"
          typeOf (Field _ ty) = ty
byteSizeOf _     (TyConapp _ _) = Nothing

-- only works on bfields, helper function to compute the ctype of a given bfield
cTypeOf :: Ty -> String
cTypeOf (BField i s _) = let sign = if s == Signed then "" else "u"
                          in sign ++ "int" ++ show i ++ "_t"
cTypeOf _              = "" -- unsafe
