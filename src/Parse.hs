{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Ast
import Test.QuickCheck
import Text.ParserCombinators.Parsec
import Data.Char

-- Utility funciton to run a parser and satisfy the given function
runParserFresh :: GenParser tok () a -> [tok] -> (a -> Bool) -> Bool
runParserFresh p input sat = 
  case runParser p () "test snippet" input of
    Right res -> sat res
    Left _ -> False

decimal :: Parser Integer
decimal = do
    digits <- many1 digit
    let n = foldl (\x d -> 10*x + toInteger (digitToInt d)) 0 digits
    seq n (return n)

reserved :: [String]
reserved = ["end", "block", "tag", "foropts"]

-- |Whitespaces that is not a newline
justSpace :: Parser Char
justSpace = satisfy $ \c -> isSpace c && c /= '\n'

justSpaces :: Parser String
justSpaces = many1 justSpace

parseName :: Parser Name
parseName = do
  first <- letter
  rest <- many (letter <|> digit <|> char '_')
  let n = first:rest
    in if n `elem` reserved
          then unexpected (n ++ " is a reserved word")
          else return n

parseTy :: Parser Ty
parseTy = parseBField <|> try parseTyConapp <|> try parseTycon <|> parseSumTy
  where
    parseOpt spaceParser =
      spaceParser >> parseTy >>= \t -> 
        spaceParser >> char '=' >> spaceParser >> decimal >>= \i ->
          return (t, i)
    parseTyConapp = do
      ty <- parseTycon
      tys <- many1 (try (justSpaces >> (parseBField <|> parseTycon)))
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

    parseSingleLineSumTy = do
      _ <- string "tag"
      tag <- justSpaces >> (parseBField <|> parseTycon)
      _ <- justSpaces >> string "foropts"
      opt <- parseOpt justSpaces
      opts <- many (justSpaces >> char '|' >> parseOpt justSpaces)
      return $ SumTy tag (opt:opts)

    parseMultiLineSumTy = do
      _ <- string "tag"
      tag <- justSpaces >> (parseBField <|> parseTycon)
      _ <- justSpaces >> string "foropts"
      _ <- justSpaces >> char '{'
      opt <- parseOpt spaces
      opts <- many (try (spaces >> char '|' >> parseOpt spaces))
      _ <- spaces >> char '}'
      return $ SumTy tag (opt:opts)

    parseSumTy = try parseSingleLineSumTy <|> parseMultiLineSumTy

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

parseEntry :: Parser Entry
parseEntry = do
  entry <- try parseField <|> (parseBlock >>= \b -> return (Blk b))
  _ <- many justSpace >> newline
  return entry

parseField :: Parser Entry
parseField = do
  n <- parseName
  _ <- spaces >> char ':' >> spaces
  ty <- parseTy
  return $ Field n ty

parseFile :: Parser [Block]
parseFile = many1 (try (spaces >> parseBlock))

prop_ParseSingleLevelBlock :: Block -> Bool
prop_ParseSingleLevelBlock b =
  runParserFresh parseBlock (pretty b) (== b)


prop_ParseDoubleLevelBlock :: Block -> Block -> Bool
prop_ParseDoubleLevelBlock (Block n (e:es)) inner =
  runParserFresh parseBlock (pretty c) (== c)
    where c = Block n (e : Blk inner : es)
prop_ParseDoubleLevelBlock (Block _ []) inner = -- just test the inner
  runParserFresh parseBlock (pretty inner) (== inner)


return []
testMod :: IO Bool
testMod = $quickCheckAll
