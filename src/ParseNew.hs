module ParseNew where
import Test.QuickCheck
import Ast

import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import Data.Text


sc :: Parser ()
sc = L.space (spaceChar >> return ()) lineComment blockComment
    where lineComment = L.skipLineComment "//"
          blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser Name
symbol = L.symbol sc

--decimal :: Parser Integer
--decimal = lexeme L.integer

int :: Parser Int
int = fromIntegral <$> L.integer

reserved :: [String]
reserved = ["end", "block", "tag", "foropts"]

-- Could do a fun thing with TemplateHaskell where I check that
-- the rword is in reserved at compile time.
rword :: String -> Parser ()
rword w = try (string w) *> notFollowedBy alphaNumChar *> sc

identifier :: Parser String
identifier = lexeme (p >>= check)
  where p = do
          c <- letterChar
          cs <- many alphaNumChar
          pure (c:cs)
        check x = if x `elem` reserved
                     then fail $ (show x) ++ " is a reserved word!"
                     else pure x

parseTy :: Parser Ty
parseTy = parseBField <|> parseSumTy <|> try parseTyConapp <|> parseTycon

parseBField :: Parser Ty
parseBField = lexeme $ do
  width <- int
           <?> "BFields length integer."
  sign <- oneOf "us"
           <?> "BFields to specify 'u' (unsigned) or 's' (signed),"
  endianness <- oneOf "bln \n\t"
           <?> "BFields to specify 'b' (big endian), 'l' (little endian), "
                ++ "or 'b' (native endian). Defaults to native endian."
  pure $ BField
      width
      (case sign of
        'u' -> Unsigned
        's' -> Signed)
      (case endianness of
        'n' -> NativeEndian
        ' ' -> NativeEndian
        '\n' -> NativeEndian
        '\t' -> NativeEndian
        'b' -> BigEndian
        'l' -> LittleEndian)

parseTycon :: Parser Ty
parseTycon = lexeme $ Tycon <$> identifier

parseTyConapp :: Parser Ty
parseTyConapp = lexeme $ do
  ty <- parseTycon
  tys <- some (parseBField <|> parseTycon)
            <?> "arguments for the " ++ pretty ty ++ " type constructor"
  pure (TyConapp ty tys)

parseSumTy :: Parser Ty
parseSumTy = lexeme $ do
  _ <- rword "tag"
  tag <- (parseBField <|> parseTycon)
  _ <- rword "foropts"
  opts <- curlies (parseOpt `sepBy1` symbol "|")
  pure $ SumTy tag opts

curlies :: Parser a -> Parser a
curlies = lexeme . between (symbol "{") (symbol "}")

parseOpt :: Parser (Ty, Integer)
parseOpt = lexeme $ do
  ty <- parseTy
  _ <- symbol "="
  tagNo <- L.integer
  pure (ty, tagNo)

prop_ParseTyParsesArbitraryType :: Ty -> Bool
prop_ParseTyParsesArbitraryType t =
  runParserTest parseTy (tpretty t) (== t)

runParserTest p input sat = 
  case runParser p "test snippet" input of
    Right res -> sat res
    Left _ -> False

return []
testMod :: IO Bool
testMod = $quickCheckAll