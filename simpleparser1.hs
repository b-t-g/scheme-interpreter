module Main where
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

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
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

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
               return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= (return . Number . read)
