
{-# LANGUAGE OverloadedStrings #-}

module GenC2
-- Uncomment the below line to expose all top level symbols for
-- repl testing
{-
(gen) 
-- -}
where

import Ast (Block(..))
import TypeCheck

-- import Control.Monad.State.Lazy
-- import Control.Monad.Reader

-- import Control.Monad
import Control.Monad.RWS.Lazy

-- import qualified Data.Text as T

import Text.PrettyPrint hiding ((<>))
import qualified CPretty
import SyntaxImp

--
-- The generator function that we expose from this module
--

gen :: Env Block -> [Block] -> String
gen gamma bs = 
    let env = GenEnv { gamma = gamma }
        (_, _, out) = runRWS (genCM bs) env initGenSt
     in printOutput out


genCM :: [Block] -> GenM ()
genCM bs = sequence (map genHandle bs) >>= \_ -> return ()


--
-- Define our state monad and initial state
--

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
    where doc = vcat $ (map CPretty.cpretty (decls out))


instance Monoid Output where
    mempty = Output {
          decls = mempty
        }
    mappend x y = Output {
          decls = decls x `mappend` decls y
        }

type GenM = RWS GenEnv Output GenSt

-- data GenM a = Reader CodeGenEnv a

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
    put $ st { counter = (counter st) + 1 }
    return $ IId base (Just (counter st))

-- makes a nekked id. beware of name capture
mkId :: [String] -> GenM IId
mkId base = return $ IId base Nothing

genHandle :: Block -> GenM ()
genHandle (Block name _) = do
    n <- mkId [name]
    isValid <- mkId ["is", "valid"]
    d <- mkId ["data"]
    tell $ Output [ IStructure n [
          IAnnId isValid (IMut IBool)
        , IAnnId d (IMut (IPtr (IMut IByte)))
        ]]

-- mkStructPtr :: IId -> IAnnTy


genRead :: Block -> GenM ()
genRead (Block name entries) = do
    funName <- mkId [name, "read", "new"]
    handle <- mkId [name]
    tell $ IFun funName -- [IAnnId handle (IMut (IStruct 
    
