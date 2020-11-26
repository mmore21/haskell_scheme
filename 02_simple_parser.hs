import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readOct, readHex)

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

escapedChar :: Parser Char
escapedChar = do
    char '\\'
    c <- oneOf ['\\', '"', 'n', 'r', 't']
    return $ case c of
        '\\' -> c
        '"' -> '\n'
        'r' -> '\r'
        't' -> '\t'
  
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapedChar <|> noneOf ['\\', '"'])
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
parseNumber = do
  x <- many1 digit
  return $ (Number . read) x

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

