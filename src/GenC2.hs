
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

import Language.C.Syntax.AST
import Language.C.Parser
import Language.C.Data.Position
import Language.C.Data.Name
import Language.C.Data.Ident
import Language.C.Data.Node
-- import qualified Language.C.Pretty as CPretty
import Data.ByteString
-- import qualified Data.Text as T


--
-- The generator function that we expose from this module
--

gen :: Env Block -> [Block] -> String
gen _ _ = "Unimplimented."

--
-- Define our state monad and initial state
--

data GenEnv = GenEnv {
            gamma :: Env Block
          , tgtFileName :: String
        }
    deriving( Show )

data GenSt = GenSt {
            counter :: Int 
          , names :: [Name]
        }
    deriving( Show )

data Output = Output {
          decls :: [CExtDecl]
        }
    deriving( Show )

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
              , tgtFileName = "BOGUS" 
            }

initGenSt :: GenSt
initGenSt = GenSt {
        counter = 0
      , names = newNameSupply
    }

mkId :: String -> GenM Ident
mkId base = do
    pos <- bogusPos
    st <- get
    let (n:ns) = names st -- safe because names is infinite
    put $ st { names = ns }
    return $ mkIdent pos base n

bogusNode :: GenM NodeInfo
bogusNode = do
    pos <- bogusPos
    return $ OnlyPos pos (pos, 0)

bogusPos :: GenM Position
bogusPos = do
    file <- tgtFileName <$> ask
    return $ position 0 file 0 0

genHandle :: Block -> GenM CStructUnion
genHandle (Block name _) = do
    n <- mkId name
    bogus <- bogusNode
    return $ CStruct CStructTag (Just n)
                 Nothing [] bogus
                 -- undefined undefined undefined
--
-- Debugging Utilities
--

parseCThing :: (P a) -> ByteString -> Either ParseError a
parseCThing parser input = 
    execParser_ parser input pos
        where pos = position 0 "test" 0 0

parseExtDecl :: ByteString -> Either ParseError CExtDecl
parseExtDecl = parseCThing extDeclP

