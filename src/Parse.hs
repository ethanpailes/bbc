{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Ast
import Test.QuickCheck
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Char

-- Utility funciton to run a parser and satisfy the given function
runParserFresh ::GenParser tok () a -> [tok] -> (a -> Bool) -> Bool
runParserFresh p input sat = 
  case runParser p () "test snippet" input of
    Right res -> sat res
    Left _ -> False

reserved :: [String]
reserved = ["end", "block"]

justSpace :: Parser Char
justSpace = satisfy (`elem` ("\t\r\f\v "::String))

justSpaces :: Parser String
justSpaces = many1 justSpace

parseName :: Parser Name
parseName = do
  first <- letter
  rest <- many (letter <|> digit)
  let n = first:rest
    in if n `elem` reserved
          then unexpected (n ++ " is a reserved word")
          else return n

parseTy :: Parser Ty
parseTy = parseBField <|> try parseTyConapp <|> parseTycon
  where
    parseTyConapp = do
      ty <- parseTycon
      tys <- many1 (try (justSpaces >> parseTycon))
      return $ TyConapp ty tys
    parseBField = do
      len <- many1 digit
      sign <- oneOf "su"
      endianness <- option 'n' $ oneOf "bln"
      return $ BField (read len)
                (if sign == 's' then Signed else Unsigned)
                (case endianness of
                  'b' -> BigEndian
                  'l' -> LittleEndian
                  _ -> NativeEndian)
    parseTycon = parseName >>= \n -> return $ Tycon n

prop_ParseTyParsesArbitraryType :: Ty -> Bool
prop_ParseTyParsesArbitraryType t =
  runParserFresh parseTy (pretty t) (== t)

parseBlock :: Parser Block
parseBlock = do
    _ <- string "block"
    n <- justSpaces >> parseName
    entries <- many1 (try (spaces >> parseEntry))
    _ <- spaces >> string "end"
    return $ Block n entries

parseEntry = do
  entry <- parseField <|> (parseBlock >>= \b -> return (Blk b))
  _ <- many justSpace >> newline
  return entry

parseField = do
  n <- parseName
  _ <- spaces >> char ':' >> spaces
  ty <- parseTy
  return $ Field n ty


prop_ParseSingleLevelBlock :: Block -> Bool
prop_ParseSingleLevelBlock b =
  runParserFresh parseBlock (pretty b) (== b)


{-
c_prop_ParseDoubleLevelBlock :: Block -> Block -> Bool
c_prop_ParseDoubleLevelBlock outer@(Block n (e:es)) inner =
  runParserFresh parseBlock (show c) (== c)
    where
      c = Block n (e : Blk inner : es)
c_prop_ParseDoubleLevelBlock outer@(Block n []) inner = -- just test the inner
  runParserFresh parseBlock (show inner) (== inner)
-}


return []
testMod :: IO Bool
testMod = $quickCheckAll
