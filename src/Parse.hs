{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Ast
import Test.QuickCheck
import Text.ParserCombinators.Parsec
import Data.Char

-- Utility funciton to run a parser and satisfy the given function
runParserFresh ::GenParser tok () a -> [tok] -> (a -> Bool) -> Bool
runParserFresh p input sat = 
  case runParser p () "test snippet" input of
    Right res -> sat res
    Left _ -> False

spaces1 :: Parser ()
spaces1 = skipMany1 spaces

parseBField :: Parser Ty
parseBField = do
  len <- many digit
  sign <- oneOf "su"
  endianness <- option 'n' $ oneOf "bln"
  return $ BField (read len)
            (if sign == 's' then Signed else Unsigned)
            (case endianness of
              'b' -> BigEndian
              'l' -> LittleEndian
              _ -> NativeEndian)

prop_BFieldPreservesInfo :: Positive Int -> Sign -> Endianness -> Bool
prop_BFieldPreservesInfo (Positive len) s e =
  runParserFresh parseBField (show len ++ show s ++ show e) (== BField len s e)

parseNamedType :: Parser Ty
parseNamedType = do
  first <- letter
  rest <- many (letter <|> digit)
  return $ Tycon (first : rest)

prop_NamedType :: Name -> Bool
prop_NamedType n = runParserFresh parseNamedType name (== Tycon name)
      where name = filter isLetter ('x':n)

parseTy :: Parser Ty
parseTy = parseBField <|> parseNamedType

prop_ParseTyDoesNotThrowOutInput :: Name -> Bool
prop_ParseTyDoesNotThrowOutInput n =
    runParserFresh parseTy name (== Tycon name)
      where name = filter isLetter ('x':n)


return []
testMod :: IO Bool
testMod = $quickCheckAll
