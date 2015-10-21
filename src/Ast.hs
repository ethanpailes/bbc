module Ast where
import Test.QuickCheck
import Control.Monad
import Data.List
import Data.Char

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
      deriving( Eq )

instance Show Ty where
  show (BField len s e) = show len ++ show s ++ show e
  show (Tycon n) = n
  show (TyConapp ty tys) = show ty ++ (' ' : unwords (map show tys))

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
  deriving( Eq )

instance Show Entry where
  show (Blk b) = show b
  show (Field n ty) = n ++ " : " ++ show ty

-- To avoid potentially infinite blocks this only generates fields
instance Arbitrary Entry where
  arbitrary = do
        n <- suchThat arbitrary (\name -> all isLetter name && not (null name))
        ty <- arbitrary
        return $ Field n ty

data Block = Block Name [Entry]
  deriving( Eq )

instance Show Block where
  show (Block n es) = "block " ++ n ++ "\n"
          ++ concatMap (\e -> "    " ++ show e ++ "\n") es ++ "end"


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
