module Ast where
import Test.QuickCheck

type Name = String
data Sign = Signed | Unsigned
  deriving( Eq )

instance Show Sign where
  show Signed = "s"
  show Unsigned = "u"

instance Arbitrary Sign where
    arbitrary = elements [Signed, Unsigned]

data Endianness = BigEndian | LittleEndian | NativeEndian
  deriving( Eq )

instance Show Endianness where
  show BigEndian = "b"
  show LittleEndian = "l"
  show NativeEndian = "n"

instance Arbitrary Endianness where
    arbitrary = elements [BigEndian, LittleEndian, NativeEndian]
      

-- BField will be the only primitive type. All others will be derived.
data Ty = BField Int Sign Endianness
        | Tycon Name
        | TyConapp Ty [Ty]
      deriving( Show, Eq )

data Entry = Block | Field Name Ty
data Block = Blk Name [Entry]
-- Issues:
--    A block should be just another type
--    I need to figure out how to encode the Convert and Num typeclasses
--        - Could do this with environments mapping type names to method
--            dictionaries (or more likely the code description that I need)


data Exp = Const Int
         | Var Name
         | Times Exp Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Div Exp Exp
-- Issue:
--    I need some way to saying that an expression can handle any a
--    such that (Num a).

-- Environments
