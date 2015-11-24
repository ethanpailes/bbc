
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
      ++ "    return true;\n"
      ++ "}\n"
      ++ concatMap (genBlock gamma) bs
      ++ "\n#endif\n"
          where headerGaurd = "BYTE_BLOCKS__" ++
                    take 20 (randomRs ('A','Z') $ unsafePerformIO newStdGen)

type GenFunc = Env Block -> Block -> String

genBlock :: GenFunc
genBlock gamma b = unlines $ map (\genFun -> genFun gamma b) genFuns
    where
      genFuns = [ genStructure
                , genSize
                , genPack
                , genUnpack
                , \_ _ -> "\n"
                , genWrite
                , genRead
                , genFree
                , \_ _ -> "\n\n"]



genStructure :: GenFunc
genStructure _ (Block n es) =
  "typedef struct " ++ n ++ " {\n"
    ++ concatMap fieldStr es
    ++ "} " ++ n ++ ";\n"
    where
      cFieldDecl cType fName = "    " ++ cType ++ (' ' : fName) ++ ";\n"

      fieldStr (Blk _) = throw $ Exceptions.Unsupported "Nested blocks."
      fieldStr (Field name ty) =
        case ty of
          (BField i _ _) ->
             if i `elem` [8, 16, 32, 64]
               then cFieldDecl (cTypeOf ty) name
               else throw $ Exceptions.Unsupported "Unaligned bitfields."
          -- based on the fact that previously defined structs will already
          -- have been defined in the output file, we can safely use them
          -- as C types.
          (Tycon tyName) -> cFieldDecl tyName name
          (TyConapp t tys) ->
            case t of
              (Tycon "array") ->
                let [tag, content] = tys
                 in fieldStr (Field (name ++ "_len") tag)
                 ++ case content of
                      (BField {}) -> cFieldDecl (cTypeOf content ++ " *") name
                      (Tycon tyName) -> "    " ++tyName++ " * " ++ name ++ ";\n"
                      (TyConapp {}) -> throw $ Exceptions.Unsupported
                                                    "Nested higher order types."
              _ -> throw Exceptions.TypeError

genSize :: GenFunc
genSize gamma blk@(Block blkName entries) =
     "int " ++ blkName ++ "_size(const " ++ blkName ++ " const * b)\n"
  ++ "{\n"
  ++ "    int size = " ++ show staticSize ++ ";\n"
  ++ (if isStatic then "" else concatMap dynamicSizeOf entries)
  ++ "    return size;\n}"
    where blkSize = blockSize gamma blk
          isStatic = isRight blkSize
          staticSize = case blkSize of
                         (Right s) -> s
                         (Left s) -> s
          dynamicSizeOf (Blk _) = throw $ Exceptions.Unsupported "Nested blocks"
          dynamicSizeOf (Field _ (BField {})) = ""
          dynamicSizeOf (Field fName (Tycon tyName)) = 
            -- We know from the typechecker that this type must be in gamma
            "    size += " ++ tyName ++ "_size(b->" ++ fName ++ ");\n"
          dynamicSizeOf (Field fName hot@(TyConapp ty tys)) =
            case ty of
              (Tycon "array") -> 
                let [tag, content] = tys
                    bSize = byteSizeOf gamma content
                 in case bSize of
                      (Just i) ->
                        "    size += (b->"++fName++"_len * " ++ show i ++ ");\n"
                      Nothing ->
                        case content of
                          (Tycon tyName) ->
                            let iter = fName ++ "_iter"
                             in "    " ++ cTypeOf tag ++ ' ' : iter ++ " = 0;\n"
                            ++ "    for("++iter++" = 0; "++iter++" < b->"++fName
                                   ++"_len; ++"++iter++") {\n"
                            ++ "        size += " ++ tyName
                                   ++ "_size(b->"++fName++" + "++iter++");\n"
                            ++ "    }\n"
                          _ -> throw Exceptions.TypeError
              _ -> throw $ Exceptions.MalformedHigherOrderType "genSize:" hot


-- Some blocks it is nice to have lying around for REPL testing
{-
b2 = Block "test2"
        [Field "f1" (BField 32 Signed BigEndian),
         Field "f2" (TyConapp (Tycon "array") [BField 16 Unsigned LittleEndian,
                                               BField 32 Signed BigEndian])]
b3 = Block "test3"
        [Field "f1" (BField 32 Signed BigEndian)]

b1 = Block "test1"
        [Field "f1" (BField 16 Signed BigEndian),
         Field "f2" (TyConapp (Tycon "array") [BField 16 Unsigned BigEndian,
                                               Tycon "test2"])]
gamma' = M.insert "test2" b2 TypeCheck.gammaInit
-}

genWrite :: GenFunc
genWrite gamma blk@(Block n _) =
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
    where blkSize = blockSize gamma blk


genPack :: GenFunc
genPack gamma (Block n entries) =
     "int " ++ n ++ "_pack(const " ++ n ++ " const *src, char *tgt)\n{\n"
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
      packStmt (Field fName (TyConapp ty tys)) =
            case ty of
              (Tycon "array") ->
                  let [tag, content] = tys
                      boundCType = cTypeOf tag
                      iteratorName = fName ++ "_iter"
                   in packStmt (Field (fName ++ "_len") tag)
                   ++ "    " ++ boundCType ++ " " ++ iteratorName ++ ";\n"
                   ++ "    for(" ++ iteratorName ++ " = 0; "
                          ++ iteratorName ++ " < src->" ++ fName ++ "_len; ++" 
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
 - The generated code for read has to use a growing buffer to figure out how
 - to read in a given byte block.
 -}
genRead :: GenFunc
genRead gamma blk@(Block blkName _) = 
  let -- breaks entries up into chunks that we can read all at once
      readSequences :: Block -> [[Entry]]
      readSequences block =
        let
          rs :: Block -> [[Entry]] -> [Entry] -> [[Entry]]
          rs (Block _ []) finished acc = reverse acc : finished
          rs (Block bName (e:es)) finished acc =
            case e of
              (Blk _) -> throw $ Exceptions.Unsupported "Nested blocks"
              (Field _ (BField {})) -> rs (Block bName es) finished (e : acc)
              (Field _ (Tycon tyName)) ->
                  let (Block _ blkEntries) = fromJust (tyName `M.lookup` gamma)
                   in readSequences (Block "BOGUS" (blkEntries ++ es ))
              (Field _ (TyConapp ty _)) ->
                case ty of
                  (Tycon "array") ->
                    rs (Block bName es) ((reverse acc : finished) ++ [[e]]) []
                  _ -> throw Exceptions.TypeError
         in filter (/= []) $ rs block [[]] []
      readSequenceStr :: ([Entry], String) -> String
      readSequenceStr ([], _) = throw Exceptions.RealityBreach
      readSequenceStr ([Field fName (TyConapp (Tycon "array")
                                              [tag, content])], rSeqTag) =
        let lenStr = fName ++ "_len"
         in readSequenceStr ([Field "BOGUS" tag], rSeqTag ++ "a")
             ++ "    " ++ cTypeOf tag ++ " " ++ lenStr ++ " = "
                            ++ endianReadFuncStr tag ++ "(*( (( "
                            ++ cTypeOf tag ++" *) (buff + used)) - 1));\n"
             ++ (case byteSizeOf gamma content of
                  (Just n) ->
                    readFixedSequence len (rSeqTag ++ "b") (len ++ " && ")
                        where len = '(' : lenStr ++ " * " ++ show n ++ ")"
                  Nothing ->
                       "    " ++ cTypeOf tag ++ ' ' : iter ++ " = 0;\n"
                    ++ "    for(" ++ iter ++ " = 0; " ++ iter
                             ++ " < " ++ lenStr ++ "; ++" ++ iter ++ ") {\n"
                    ++ unlines -- when Haskell turns into lisp
                        (map ("    " ++)
                          (lines (concatMap readSequenceStr
                            (zip (readSequences childBlock)
                              (map (\i -> rSeqTag ++ tyName ++ show i)
                                [(0 :: Integer) .. ])))))
                    ++ "    }\n"
                        where iter = fName ++ "_iter"
                              (Tycon tyName) = content
                              childBlock = fromJust (tyName `M.lookup` gamma))

      readSequenceStr (es, rSeqTag) =
        case blockSize gamma (Block "BOGUS" es) of
          (Right n) -> readFixedSequence (show n) rSeqTag ""
          (Left _) -> throw Exceptions.RealityBreach
      readFixedSequence :: String -> String -> String -> String
      readFixedSequence len tag readGaurd =  
           tag ++ ":\n"
        ++ "    if (used + " ++ len ++ " > buff_len) {\n"
        ++ "        grow_buff(&buff, &buff_len);\n"
        ++ "        goto " ++ tag ++ ";\n"
        ++ "    } else {\n"
        ++ "        if (" ++ readGaurd ++ "fread(buff + used, " ++ len
                        ++ ", 1, f) != 1) return false;\n"
        ++ "        used += " ++ len ++ ";\n"
        ++ "    }\n"
   in
     "int " ++ blkName ++ "_read_new(" ++ blkName ++ " *tgt, FILE *f)\n"
  ++ "{\n"
  ++ case blockSize gamma blk of
       (Right bs) -> "    size_t blk_size = " ++ show bs ++ ";\n"
              ++ "    char buff[blk_size];\n"
              ++ "    if (fread(buff, blk_size, 1, f) != 1) return false;\n"
              ++ "    return " ++ blkName ++ "_unpack_new(tgt, buff);\n}"
       (Left _) ->
                 "    size_t buff_len = " ++ show readInitialBufferSize ++ ";\n"
              ++ "    size_t used = 0;\n"
              ++ "    char *buff = malloc(buff_len);\n"
              ++ concatMap readSequenceStr
                    (zip (readSequences blk)
                       (map (\i -> blkName ++ "RSEQ" ++ show i)
                            [(0 :: Integer) .. ]))
              ++ "    int ret = " ++ blkName ++ "_unpack_new(tgt, buff);\n"
              ++ "    free(buff);\n"
              ++ "    return ret;\n}"


genUnpack :: GenFunc
genUnpack gamma (Block n entries) = 
     "int " ++ n ++ "_unpack_new(" ++ n ++ " *tgt, const char const *src)\n{\n"
  ++ "    size_t bytes_consumed = 0;\n"
  ++ unpackStmts entries
  ++ "\n    return bytes_consumed;\n}"
    where
      unpackStmt (Blk _) = throw $ Exceptions.Unsupported "Nested blocks."
      unpackStmt (Field fName ty@(BField i s _)) =
        let wordSizeStr = show i
            signStr = case s of
                        Signed -> ""
                        Unsigned -> "u"
            endianFuncStr = endianReadFuncStr ty
            wordPtrStr = '(' : signStr ++ "int" ++ wordSizeStr ++ "_t*)"
         in if i `elem` [8, 16, 32, 64]
              then "    tgt->" ++ fName
                  ++ " = " ++ endianFuncStr ++ "(* (" ++ wordPtrStr
                  ++ "(src + bytes_consumed))); "
                  ++ "bytes_consumed += "
                  ++ show (fromJust (byteSizeOf gamma ty))
                  ++ ";\n"
              else throw $ Exceptions.Unsupported "Unaligned bitfields."

      unpackStmt (Field fName (Tycon tyName)) =
            "    bytes_consumed += " ++ tyName ++ "_unpack_new(&(tgt->"
                ++ fName ++ "), (src + bytes_consumed));\n"
      unpackStmt (Field fName (TyConapp ty tys)) =
        case ty of
          (Tycon "array") ->
            let [tag, content] = tys
                iteratorName = fName ++ "_iter"
             in "    " ++ cTypeOf tag ++  ' ' : iteratorName ++ " = 0;\n"
             ++ unpackStmt (Field (fName ++ "_len") tag)
             ++ (case content of
                  (BField {}) ->
                     "    tgt->" ++ fName ++ " = malloc(tgt->"
                                 ++ fName ++ "_len * "
                    ++ show (fromJust (byteSizeOf gamma content)) ++ ");\n"
                  (Tycon tyName) ->
                       "    tgt->" ++ fName ++ " = malloc(tgt->"
                              ++ fName ++ "_len * sizeof(" ++ tyName ++ "));\n"
                  _ -> throw (Exceptions.Unsupported
                             "Nested Higher Order types."))
             ++ "    for(" ++ iteratorName ++ " = 0; "
                        ++ iteratorName ++ " < tgt->"
                        ++ fName ++ "_len; ++" ++ iteratorName ++ ") {\n"
             ++ "    " ++ unpackStmt
                      (Field (fName ++ '[' : iteratorName ++ "]") content)
             ++ "    }\n"
          _ -> throw Exceptions.TypeError
      unpackStmts [] = ""
      unpackStmts (e:es) =
        case e of
          (Blk _) -> throw $ Exceptions.Unsupported "Nested Blocks."
          field -> unpackStmt field ++ unpackStmts es


genFree :: GenFunc
genFree _ (Block bName entries) =
  let freeStr (Blk _) = throw $ Exceptions.Unsupported "Nested Blocks."
      freeStr (Field _ (BField {})) = Nothing
      freeStr (Field fName (Tycon tyName)) = 
        Just $ "    " ++ tyName ++ "_free(&(tgt->" ++ fName ++ "));\n"
      freeStr (Field fName (TyConapp (Tycon "array") [tag, content])) =
        Just 
          ((case content of
            (BField {}) -> "" -- No need to free each element
            (Tycon tyName) ->
                 "    " ++ cTypeOf tag ++ ' ' : iter ++ " = 0;\n"
              ++ "    for(" ++ iter ++ " = 0; " ++ iter ++ " < "
                         ++ "tgt->" ++ fName ++ "_len; ++" ++ iter ++ ") {\n"
              ++ "        " ++ tyName ++ "_free(tgt->"
                              ++ fName ++ " + " ++ iter ++ ");\n"
              ++ "    }\n"
              where iter = fName ++ "_iter"
            (TyConapp {}) -> throw Exceptions.TypeError)
          ++ "    free(tgt->" ++ fName ++ "); tgt->" ++ fName ++ " = NULL;\n")
      freeStr (Field _ hot@(TyConapp _ _)) = 
        throw $ Exceptions.MalformedHigherOrderType "genFree:" hot
   in
     "void " ++ bName ++ "_free(" ++ bName ++ " *tgt)\n"
  ++ "{\n"
  ++ concat (mapMaybe freeStr entries)
  ++ "}"


-- only take a BField as the type argument
endianReadFuncStr :: Ty -> String
endianReadFuncStr (BField i _ BigEndian) =
  if i <= 8 then "" else "be" ++ show i ++ "toh"
endianReadFuncStr (BField i _ LittleEndian) = 
  if i <= 8 then "" else "le" ++ show i ++ "toh"
endianReadFuncStr (BField _ _ NativeEndian) = ""
endianReadFuncStr _ = throw Exceptions.RealityBreach

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
      sizeOfType (Field _ (TyConapp t tys)) =
            case t of
              (Tycon "array") ->
                let [tag, _] = tys
                 in case sizeOfType (Field "" tag) of
                      (Right x) -> Left x
                      (Left _) -> throw Exceptions.RealityBreach -- not possible
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
          typeOf (Blk _) = throw $ Exceptions.Unsupported "Nested blocks."
          typeOf (Field _ ty) = ty
byteSizeOf _     (TyConapp _ _) = Nothing

-- only works on bfields, helper function to compute the ctype of a given bfield
cTypeOf :: Ty -> String
cTypeOf (BField i s _) = let sign = if s == Signed then "" else "u"
                          in sign ++ "int" ++ show i ++ "_t"
cTypeOf (Tycon tyName) = tyName
cTypeOf _              = "" -- unsafe
