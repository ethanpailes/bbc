
module Exceptions where
import Ast
import Control.Exception
import Data.Typeable

{-
 - Exceptions are not used as control flow in the Byte Blocks compiler, they
 - do offer a convenient way to fail hard with an informative message though.
 -}

data CompilerException = Unsupported String
                       | UnknownBlock Name
                       | TypeError
                       | UnknownTypeConstructor Name
                       | MalformedHigherOrderType Ty
  deriving( Typeable )

instance Show CompilerException where
  show (Unsupported s) = "Unsupported langauge feature: " ++ s
  show (UnknownBlock n) = "Unknown block: " ++ n
  show TypeError = "Type error. Bug in type checker."
  show (UnknownTypeConstructor n) = "Unknown Type Constructor " ++ n
  show (MalformedHigherOrderType ty) = "Malformed Higher Order Type " ++ show ty

instance Exception CompilerException



data ArgException = UnknownTgtLang String
  deriving( Typeable )

instance Show ArgException where
  show (UnknownTgtLang s) = "Unknown target langauge: " ++ s

instance Exception ArgException

