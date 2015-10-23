module Ast where
import Test.QuickCheck
import Control.Monad
import Data.List
import Data.Char

class Pretty a where
  pretty :: a -> String

type Name = String
data Sign = Signed | Unsigned
  deriving( Eq, Show )

instance Pretty Sign where
  pretty Signed = "s"
  pretty Unsigned = "u"

instance Arbitrary Sign where
    arbitrary = elements [Signed, Unsigned]

data Endianness = BigEndian | LittleEndian | NativeEndian
  deriving( Eq, Show )

instance Pretty Endianness where
  pretty BigEndian = "b"
  pretty LittleEndian = "l"
  pretty NativeEndian = "n"

instance Arbitrary Endianness where
    arbitrary = elements [BigEndian, LittleEndian, NativeEndian]

-- BField will be the only primitive type. All others will be derived.
data Ty = BField Int Sign Endianness
        | Tycon Name
        | TyConapp Ty [Ty]
      deriving( Eq, Show )

instance Pretty Ty where
  pretty (BField len s e) = show len ++ pretty s ++ pretty e
  pretty (Tycon n) = n
  pretty (TyConapp ty tys) = pretty ty ++ (' ' : unwords (map pretty tys))

instance Arbitrary Ty where
  arbitrary = oneof [aTycon, aBFeild, aTyConapp]
    where
      aBFeild = do
        len <- suchThat arbitrary (> 0)
        s <- arbitrary
        e <- arbitrary
        return $ BField len s e
      aTycon = liftM Tycon (suchThat arbitrary
                              (\list -> all isLetter list && not (null list)))
      aTyConapp = do
        ty <- aTycon
        tys <- suchThat (listOf aTycon) (not . null)
        return $ TyConapp ty tys

data Entry = Blk Block | Field Name Ty
  deriving( Eq, Show )

instance Pretty Entry where
  pretty (Blk b) = pretty b
  pretty (Field n ty) = n ++ " : " ++ pretty ty

-- To avoid potentially infinite blocks this only generates fields
instance Arbitrary Entry where
  arbitrary = do
        n <- suchThat arbitrary (\name -> all isLetter name && not (null name))
        ty <- arbitrary
        return $ Field n ty

data Block = Block Name [Entry]
  deriving( Eq, Show )

instance Pretty Block where
  pretty (Block n es) = "block " ++ n ++ "\n"
          ++ concatMap (\e -> "    " ++ pretty e ++ "\n") es ++ "end"

instance Arbitrary Block where
  arbitrary = do
    n <- suchThat arbitrary (\l -> all isLetter l && not (null l))
    entries <- suchThat (listOf arbitrary) (not . null)
    return $ Block n entries


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
