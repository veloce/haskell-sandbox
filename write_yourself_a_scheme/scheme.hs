module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Char Char

main = do
    args <- getArgs
    putStrLn $ readExpr (args !! 0)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

escChar :: Parser Char
escChar = char '\\' >> oneOf "\\\"tnr"

parseString :: Parser LispVal
parseString = do
    char '"'
    let valid = noneOf "\"" <|> escChar
    x <- many valid
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
    first <- char '#'
    second <- oneOf "bdox"
    rest <- many digit
    let prefix = first:[second]
    return $ case prefix of
        "#o" -> Number num
            where [(num, _)] = readOct rest
        "#x" -> Number num
            where [(num, _)] = readHex rest
        "#d" -> Number num
            where [(num, _)] = readDec rest
    <|> (liftM (Number . read) $ many1 digit)

parseChar :: Parser LispVal
parseChar = do
    char '#'
    char '\\'
    x <- anyChar
    return $ Char x

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseChar
        <|> parseString
        <|> parseNumber

