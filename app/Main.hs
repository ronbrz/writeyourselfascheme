module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

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
            <|> parseQuoted
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

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents)= "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                         if null parsed
                            then 0
                            else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
