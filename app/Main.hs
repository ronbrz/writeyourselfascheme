module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value" ++ show val

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many ((char '\\' >> oneOf "nrt\\\"" ) <|> noneOf "\"" ) -- only returns char, return \ too?
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseNumberBase <|> (many1 digit >>= (return . Number . read))
-- try combinator seems useful, or just structure parsers without needing lookahead in single function
parseNumberBase :: Parser LispVal
parseNumberBase = do
  char '#'
  numType <- oneOf "odx"
  num <- many1 alphaNum
  return $ case numType of
    'o' -> Number $ fst $ readOct num !! 0
    'd' -> Number $ read num
    'x' -> Number $ fst $ readHex num !! 0

-- have to combine parseNumberbase and parsecharacter
-- have to add multichar character mappings and space mapping
parseCharacter :: Parser LispVal
parseCharacter = do
  char '#'
  char '\\'
  firstC <- alphaNum
  rest <- many alphaNum
  return $ Character firstC

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

spaces :: Parser ()
spaces = skipMany1 space

-- trys seem to be able to be factored out to be more efficient
parseExpr :: Parser LispVal
parseExpr = try parseCharacter
            <|> try parseNumber
            <|> parseAtom
            <|> parseString
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
  | Float Float
  deriving (Show)

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn expr
  putStrLn (readExpr expr)
