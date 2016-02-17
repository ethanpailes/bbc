

module Parse (
   parseFile
 , testMod
 ) where

import Test.QuickCheck
import Ast
import Data.Char
import Data.Either
import Exceptions
import Control.Monad
import qualified Control.Exception as E

import Text.Megaparsec
import Text.Megaparsec.Pos
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Text as T

parseFile ::
    String -> -- the name of the source file
    T.Text -> -- the text input stream from the file
    Either ParseError [Block]
parseFile sourceFile inputStream = reverse <$> parseRest [] initialState
  where initialState = State inputStream (newPos sourceFile 1 1) defaultTabWidth
        paddedParseBlock = lexemeN $ scWithNewlines >> parseBlock
        parseRest acc state =
          if isRight $ snd $ runParser' eof state -- if we are at EOF
             then Right acc else
          case runParser' paddedParseBlock state of
            (state', Right block) -> parseRest (block:acc) state'
            (_, Left e) -> Left e

prop_ParseBlockList :: [Block] -> Bool
prop_ParseBlockList bs = 
  case parseFile "test" fileTest of
    (Left _) -> False
    (Right x) -> x == bs
    where fileTest = T.intercalate "\n\n" $ map tpretty bs

sc :: Parser ()
sc = L.space (void spaceNoNewline) lineComment blockComment
    where lineComment = L.skipLineComment "//"
          blockComment = L.skipBlockComment "/*" "*/"
          spaceNoNewline = satisfy $ liftM2 (&&) (/= '\n') isSpace

scWithNewlines :: Parser ()
scWithNewlines = L.space (void spaceChar) lineComment blockComment
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
          c <- (letterChar <|> char '_')
          cs <- many (alphaNumChar <|> char '_')
          pure (c:cs)
        check x = if x `elem` reserved
                     then fail $ show x ++ " is a reserved word!"
                     else pure x

parseTy :: Parser Ty
parseTy = parseBField <|> parseSumTy <|> parseFixedArray
       <|> try parseTyConapp <|> parseTycon

parseBField :: Parser Ty
parseBField = lexeme $ do
  width <- int
           <?> "BFields length integer."
  sign <- oneOf "us"
           <?> "BFields to specify 'u' (unsigned) or 's' (signed),"
  endianness <- lookAhead (char '\n') <|> oneOf "bln \t"
           <?> "BFields to specify 'b' (big endian), 'l' (little endian), "
                ++ "or 'b' (native endian). Defaults to native endian."
  pure $ BField
      width
      (case sign of
        'u' -> Unsigned
        's' -> Signed
        _   -> E.throw (RealityBreach "Likely a bug in Text.Megaparsec"))
      (case endianness of
        'n'  -> NativeEndian
        ' '  -> NativeEndian
        '\n' -> NativeEndian
        '\t' -> NativeEndian
        'b'  -> BigEndian
        'l'  -> LittleEndian
        _    -> E.throw (RealityBreach "Likely a bug in Text.Megaparsec"))


parseFixedArray :: Parser Ty
parseFixedArray = lexeme $ do
  _ <- rword "arrayf"
  ty <- parseTy
  num <- decimal
  pure $ FixedArray ty (fromIntegral num)

parseTycon :: Parser Ty
parseTycon = lexeme (Tycon <$> identifier)

parseTyConapp :: Parser Ty
parseTyConapp = lexeme $ do
  ty <- parseTycon
  tys <- some (parseBField <|> parseTycon)
            <?> "arguments for the " ++ pretty ty ++ " type constructor"
  pure (TyConapp ty tys)

parseSumTy :: Parser Ty
parseSumTy = lexeme $ do
  _ <- rword "tag"
  tag <- parseBField <|> parseTycon
  _ <- rword "foropts"
  opts <- curlies (parseOpts scWithNewlines) <|> parseOpts sc
  pure $ SumTy tag opts

curlies :: Parser a -> Parser a
curlies = lexeme . between (symbol "{") (symbol "}")

parseOpts :: Parser () -- the whitespace consumer to be used
          -> Parser [(Ty, Integer)]
parseOpts whitespace =
  (parseOpt `sepBy` symbol "|") >>= \o -> whitespace >> pure o

parseOpt :: Parser (Ty, Integer)
parseOpt = do
  ty <- parseTy
  _ <- symbol "="
  tagNo <- decimal
  pure (ty, tagNo)

prop_ParseTyParsesArbitraryType :: Ty -> Bool
prop_ParseTyParsesArbitraryType t =
  runParserTest parseTy (tpretty t) (== t)


parseBlock :: Parser Block
parseBlock = lexeme $ do
  _ <- rword "block"
  blockName <- lexemeN identifier
  entries <- many parseEntry
  _ <- rword "end"
  pure (Block blockName entries)

parseEntry :: Parser Entry
parseEntry = lexemeN $
  (try (parseBlock >>= \b -> pure (Blk b)) <|> parseField) >>= \e ->
                                                  sc >> newline >> sc >> pure e

parseField :: Parser Entry
parseField = lexeme $ do
  fieldName <- try identifier
  _ <- lexeme (char ':')
  ty <- parseTy
  return (Field fieldName ty)

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

runParserTest :: Stream s t => Parsec s a -> s -> (a -> Bool) -> Bool
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
