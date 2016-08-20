module Main where
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric

main :: IO ()
main = do args <- getArgs
          putStrLn (args !! 0)
          putStrLn (readExpr (args !! 0))

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapeSequences :: [Char]
escapeSequences = ['\\', '"', '\n', '\r', '\t']

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

readString :: String -> String
readString input = case parse parseString "lisp" input of
    Left err -> show err
    Right val -> "found it"
 

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseBool

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ many1 (noneOf "\"\\") <|> escapeSequence
                 char '"'
                 return $ String $ concat x

escapeSequence :: Parser String
escapeSequence = char '\\' >>
                    oneOf "\\\"ntr" >>= \sequence ->
                    case sequence of
                        '\\' -> do return [sequence]
                        '"' -> do return [sequence]
                        'n' -> do return "\n"
                        't' -> do return "\t"
                        'r' -> do return "\r"

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ Atom atom

parseNumber :: Parser LispVal
parseNumber = parseHex <|> parseDecimal <|> parseOct <|> parseBinary

parseHex :: Parser LispVal
parseHex = string "#x" >> many1 hexDigit >>= (return . Number . convert readHex)

parseDecimal :: Parser LispVal
parseDecimal = try $ parseConventionalDecimal <|> parseTaggedDecimal

parseConventionalDecimal :: Parser LispVal
parseConventionalDecimal = many1 digit >>= (return . Number . read)

parseTaggedDecimal :: Parser LispVal
parseTaggedDecimal = string "#d" >> parseConventionalDecimal

parseOct :: Parser LispVal
parseOct = string "#o" >> many1 octDigit >>= (return . Number . convert readOct)

parseBinary :: Parser LispVal
parseBinary = string "#b" >> many1 (oneOf ['0', '1']) >>= return . Number . convertBinary

convert :: (Eq a, Num a, Read a) => ReadS a -> String -> a
convert base num = fst $ base num !! 0

convertBinary :: String -> Integer
convertBinary = convertBinaryAuxiliary 0

convertBinaryAuxiliary :: Integer -> String -> Integer
convertBinaryAuxiliary digInt "" = digInt

convertBinaryAuxiliary digInt (x:xs) = let old = 2*digInt + (read [x])::Integer in
                                       convertBinaryAuxiliary old xs

parseBool :: Parser LispVal
parseBool = char '#' >>
            oneOf "tf" >>=
            \boolean -> return $ case boolean of
            't' -> Bool True
            'f' -> Bool False
