module Ast where
import Test.QuickCheck
import Control.Monad
import Data.List
import Data.List.Utils
import Data.Char

class Pretty a where
  pretty :: a -> String 
type Name = String
data Sign = Signed | Unsigned
  deriving( Eq, Ord, Show )

instance Pretty Sign where
  pretty Signed = "s"
  pretty Unsigned = "u"

instance Arbitrary Sign where
    arbitrary = elements [Signed, Unsigned]

data Endianness = BigEndian | LittleEndian | NativeEndian
  deriving( Eq, Ord, Show )

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
        | SumTy Ty [(Ty, Integer)]
      deriving( Eq, Ord, Show )

isSumTy :: Ty -> Bool
isSumTy (SumTy {}) = True
isSumTy _ = False

isBField :: Ty -> Bool
isBField (BField {}) = True
isBField _ = False

instance Pretty Ty where
  pretty (BField len s e) = show len ++ pretty s ++ pretty e
  pretty (Tycon n) = n
  pretty (TyConapp ty tys) = pretty ty ++ (' ' : unwords (map pretty tys))
  pretty (SumTy tag options) =
    let prettyOpts (ty, code) = pretty ty ++ " = " ++ show code
     in "tag " ++ pretty tag ++ " foropts { " 
          ++ intercalate "\n | " (map prettyOpts options)
          ++ "\n }"
{-
tag = BField 16 Unsigned BigEndian
opt1 = BField 8 Unsigned BigEndian
opt2 = BField 32 Signed NativeEndian
s = SumTy tag [opt1, opt2]
-}

instance Arbitrary Ty where
  arbitrary = oneof [aTycon, aBFeild, aTyConapp, aSumTy]
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
      aSumTy = do
        tag <- aBFeild
        opts <- listOf1 $ oneof [aBFeild, aTycon]
        return $ SumTy tag (zip opts [0..])


data Entry = Blk Block | Field Name Ty
  deriving( Eq, Ord, Show )

instance Pretty Entry where
  pretty (Blk b) = replace "\n" "\n    " (pretty b) -- handle nesting
  pretty (Field n ty) = n ++ " : " ++ pretty ty

-- To avoid potentially infinite blocks this only generates fields
instance Arbitrary Entry where
  arbitrary = do
        n <- suchThat arbitrary (\name -> all isLetter name && not (null name))
        ty <- arbitrary
        return $ Field n ty

data Block = Block Name [Entry]
  deriving( Eq, Ord, Show )

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
