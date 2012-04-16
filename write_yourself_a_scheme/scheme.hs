module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Char

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
parseNumber =
    (parsePrefixed 'd' readDec)
    <|> (parsePrefixed 'o' readOct)
    <|> (parsePrefixed 'x' readHex)
    <|> (parsePrefixed 'b' readBin)
    <|> (liftM (Number . read) $ many1 digit)

readBin :: (Integral a) => ReadS a
readBin = readInt 2 (\x -> x == '0' || x == '1') digitToInt

parsePrefixed :: Char -> ReadS Integer -> Parser LispVal
parsePrefixed prefix readFunc = do
    char '#'
    char prefix
    digits <- many1 digit
    let [(num, _)] = readFunc digits
    return $ Number num

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

