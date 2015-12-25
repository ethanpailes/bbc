module ParseNew where
import Test.QuickCheck
import Ast
import Data.Char

import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Text as T


parseFile :: Parser [Block]
parseFile = scWithNewlines >> many parseBlock


prop_ParseBlockList :: [Block] -> Bool
prop_ParseBlockList bs = 
  runParserTest parseFile fileTest (== bs)
    where fileTest = T.intercalate "\n\n" $ map tpretty bs


-- predicate-and. I can't beleive that this isn't in the Prelude.
pand :: (a -> Bool) -> (a -> Bool) -> a -> Bool
pand f g x = f x && g x

sc :: Parser ()
sc = L.space (spaceNoNewline >> return ()) lineComment blockComment
    where lineComment = L.skipLineComment "//"
          blockComment = L.skipBlockComment "/*" "*/"
          spaceNoNewline = satisfy $ (/= '\n') `pand` isSpace

scWithNewlines :: Parser ()
scWithNewlines = L.space (spaceChar >> return ()) lineComment blockComment
    where lineComment = L.skipLineComment "//"
          blockComment = L.skipBlockComment "/*" "*/"


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexemeN :: Parser a -> Parser a
lexemeN = L.lexeme scWithNewlines

symbol :: String -> Parser Name
symbol = L.symbol sc

decimal :: Parser Integer
decimal = lexeme L.integer

int :: Parser Int
int = fromIntegral <$> L.integer


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
parseTycon = lexeme (Tycon <$> identifier)

parseTyConapp :: Parser Ty
parseTyConapp = lexeme $ do
  ty <- parseTycon
  tys <- some (parseBField <|> parseTycon)
            <?> "arguments for the " ++ pretty ty ++ " type constructor"
  pure (TyConapp ty tys)

-- TODO add support for single line
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
  tagNo <- decimal
  pure (ty, tagNo)

prop_ParseTyParsesArbitraryType :: Ty -> Bool
prop_ParseTyParsesArbitraryType t =
  runParserTest parseTy (tpretty t) (== t)


parseBlock :: Parser Block
parseBlock = lexemeN $ do
  _ <- rword "block"
  blockName <- lexemeN identifier
  entries <- some parseEntry
  _ <- rword "end"
  pure $ Block blockName entries

parseEntry :: Parser Entry
parseEntry = lexemeN $ try (parseBlock >>= \b -> pure (Blk b)) <|> parseField


parseField :: Parser Entry
parseField = lexemeN $ do
  fieldName <- try identifier
  _ <- lexeme $ char ':'
  ty <- parseTy
  return $ Field fieldName ty

prop_ParseEntryParsesArbitraryField :: Entry -> Bool
prop_ParseEntryParsesArbitraryField (Blk {}) = True -- ignore
prop_ParseEntryParsesArbitraryField f =
  runParserTest parseEntry (tpretty f) (== f)


prop_ParseSingleLevelBlock :: Block -> Bool
prop_ParseSingleLevelBlock b =
  runParserTest parseBlock (tpretty b) (== b)

prop_ParseDoubleLevelBlock :: Block -> Block -> Bool
prop_ParseDoubleLevelBlock (Block n (e:es)) inner =
  runParserTest parseBlock (tpretty c) (== c)
    where c = Block n (e : Blk inner : es)
prop_ParseDoubleLevelBlock (Block _ []) inner = -- just test the inner
  runParserTest parseBlock (tpretty inner) (== inner)

runParserTest p input sat = 
  case runParser p "test snippet" input of
    Right res -> sat res
    Left _ -> False


testMod :: IO ()
testMod =
     putStrLn "prop_ParseTyParsesArbitraryType"
  >> quickCheck prop_ParseTyParsesArbitraryType
  >> putStrLn "prop_ParseEntryParsesArbitraryField"
  >> quickCheck prop_ParseEntryParsesArbitraryField
  >> putStrLn "prop_ParseSingleLevelBlock"
  >> quickCheckWith stdArgs{ maxSize = 25 }
              prop_ParseSingleLevelBlock
  >> putStrLn "prop_ParseDoubleLevelBlock"
  >> quickCheckWith stdArgs{ maxSize = 25 }
              prop_ParseDoubleLevelBlock
  >> putStrLn "prop_ParseBlockList"
  >> quickCheckWith stdArgs{ maxSize = 15 }
              prop_ParseBlockList
