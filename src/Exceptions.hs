
module Exceptions where
import Ast
import Control.Exception
import Data.Typeable

data CompilerException = Unsupported String | UnknownBlock Name | TypeError
  deriving( Typeable )

instance Show CompilerException where
  show (Unsupported s) = "Unsupported langauge feature: " ++ s
  show (UnknownBlock n) = "Unknown block: " ++ n
  show TypeError = "Type error."

instance Exception CompilerException


