
{-# LANGUAGE OverloadedStrings #-}

module GenC2
-- Uncomment the below line to expose all top level symbols for
-- repl testing
{-
(gen) 
-- -}
where

import Ast (Block(..), Entry(..), Ty(..), Sign(..), Endianness(..))
import TypeCheck

-- import Control.Monad.State.Lazy
-- import Control.Monad.Reader
import Data.Maybe
import Control.Applicative
-- import Control.Monad
import Control.Monad.RWS.Lazy

-- import qualified Data.Text as T

import Text.PrettyPrint hiding ((<>))
import qualified CPretty

import SyntaxImp (IAnnTy(..), IAnnId(..), failureShortCircuit, IExpr(..),
                  getsStmt, (|->), ITy(..), intE, IDecl(..), IId(..), IStmt(..), mkMut)

--
-- The generator function that we expose from this module
--

gen :: Env Block -> [Block] -> String
gen gamma bs = 
    let env = GenEnv { gamma = gamma }
        (_, _, out) = runRWS (genCM bs) env initGenSt
     in printOutput out

genCM :: [Block] -> GenM ()
genCM bs = mapM genAll bs >>= \_ -> return ()

--
-- Define our state monad and initial state
--

genAll :: Block -> GenM ()
genAll b = do
  genHandle b
  genRead b

data GenEnv = GenEnv {
            gamma :: Env Block
        }
    deriving( Show )

data GenSt = GenSt {
            counter :: Int 
        }
    deriving( Show )

data Output = Output {
          decls :: [IDecl]
        }
    deriving( Show )

printOutput :: Output -> String
printOutput out = show doc
    where doc = vcat (map CPretty.cpretty (decls out))


instance Monoid Output where
    mempty = Output {
          decls = mempty
        }
    mappend x y = Output {
          decls = decls x `mappend` decls y
        }

type GenM = RWS GenEnv Output GenSt

-- it is useful to leave this lying around for repl testing
bogusGenEnv :: GenEnv
bogusGenEnv = GenEnv {
                gamma = gammaInit
            }
initGenSt :: GenSt
initGenSt = GenSt {
          counter = 0
        }

uniqueId :: [String] -> GenM IId
uniqueId base = do
    st <- get
    put $ st { counter = counter st + 1 }
    return $ IId base (Just (counter st))

byteBlobPtr :: ITy
byteBlobPtr = IPtr (IMut IByte)

-- makes a nekked id. beware of name capture
mkId :: [String] -> GenM IId
mkId base = return $ IId base Nothing

genHandleId :: Block -> GenM (IId, IAnnTy)
genHandleId (Block name _) = do
  ident <- mkId [name]
  let handleType = INoAnn (IPtr (IMut (IStruct ident)))
  return (ident, handleType)

genHandle :: Block -> GenM ()
genHandle blk = do
    (name, _) <- genHandleId blk
    isValid <- mkId ["is", "valid"]
    dataPtr <- mkId ["data"]
    parent <- mkId ["parent"]
    tell $ Output [ IStructure name [
          IAnnId isValid (IMut IBool)
        , IAnnId parent (IMut byteBlobPtr)
        , IAnnId dataPtr (IMut byteBlobPtr)
        ]]

sizeOfName :: Block -> GenM IId
sizeOfName (Block name _) = mkId [name, "sizeof"]
genSizeOf :: Block -> GenM ()
genSizeOf blk@(Block name _) = do
    funName <- sizeOfName blk

    argHandle <- uniqueId [name, "handle"]
    (_, handleTy) <- genHandleId blk
    let handle = IAnnId argHandle handleTy

    blkIsStatic <- isStatic blk
    staticSize <- fromJust <$> staticSizeOf blk

    let funArgs = [handle]
        funRet = IMut ISize
        dynamicFunBody =
          [
            IExpload "dyn fun size unimplimented."
          ]
        staticFunBody =
          [ -- TODO(ethan): this is wrong
            IReturn (intE staticSize)
          ]
    tell $ Output [IFun funName funArgs funRet
                   (if blkIsStatic then staticFunBody else dynamicFunBody)]

readName :: Block -> GenM IId
readName (Block name _) = mkId [name, "read", "new"]
genRead :: Block -> GenM ()
genRead blk@(Block name _) = do
    funName <- readName blk

    f <- mkId ["file"]
    let file = IAnnId f (INoAnn IFile)

    (handleId, handleTy) <- genHandleId blk
    let handleAnId = IAnnId handleId (mkMut handleTy)

    blkIsStatic <- isStatic blk
    staticSize <- fromJust <$> staticSizeOf blk

    malloc <- mkId ["malloc"]
    fread <- mkId ["fread"]
    dataField <- mkId ["data"]
    parentField <- mkId ["parent"]
    isValidField <- mkId ["is", "valid"]


    let funArgs = [file, handleAnId]
        funRet = IMut IBool
        dynamicFunBody =
          [
            IExpload "dyn read unimplimented."
          ]
        staticFunBody =
          [
            getsStmt (handleId |-> dataField)
                     (IEFunCall malloc [intE staticSize])
          , getsStmt (handleId |-> parentField)
                     (IEZero (IPtr handleTy))
          , getsStmt (handleId |-> isValidField) IETrue
          , failureShortCircuit $ IEFunCall
            fread [IEId (handleId |-> dataField), intE staticSize, intE 1, IEId f]
          ]
    tell $ Output [IFun funName funArgs funRet
                  (if blkIsStatic then staticFunBody else dynamicFunBody)]

---
--- Utilities
---

isStatic :: Block -> GenM Bool
isStatic = fmap isJust . staticSizeOf

-- | the static size of a block. If the block is dynamicly sized
--   the dynamic size will not be included in this count.
staticSizeOf :: Block -> GenM (Maybe Int)
staticSizeOf (Block _ entries) = do
  sizedEntries <- mapM entryStaticSize entries
  return $ foldr (liftA2 (+)) (pure 0) sizedEntries 

entryStaticSize :: Entry -> GenM (Maybe Int)
entryStaticSize (Blk _) = return Nothing
entryStaticSize (Field _ ty) = tyStaticSize ty

tyStaticSize :: Ty -> GenM (Maybe Int)
tyStaticSize (BField i _ _ ) = return $ Just (i `div` 8)
tyStaticSize _ = return Nothing -- TODO(ethan): nested blocks
-- tyStaticSize (Tycon _) = return Nothing

--
-- REPL TESTING
--

-- produce all the output for a block
genTest :: Block -> String
genTest b = gen gammaInit [b]

genTestOne :: Block -> (Block -> GenM ()) -> String
genTestOne b g =
  let env = GenEnv { gamma = gammaInit }
      (_, _, out) = runRWS (g b) env initGenSt
   in "\n\n\n" ++ printOutput out ++ "\n\n\n"

testBlock :: Block
testBlock = Block "test"
            [
              Field "f1" (BField 16 Unsigned LittleEndian)
            , Field "f2" (BField 8 Signed BigEndian)
            ]
