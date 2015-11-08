
module Exceptions where
import Control.Exception
import Data.Typeable

data CompilerException = Unsupported String
  deriving( Typeable )

instance Show CompilerException where
  show (Unsupported s) = "Unsupported langauge feature: " ++ s

instance Exception CompilerException


