module Main where
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric

main :: IO ()
main = getArgs >>= \args -> putStrLn (readExpr (args !! 0))

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Complex Double Double 
             | Number Integer
             | Float Double
             | Rational Integer Integer
             | String String
             | Bool Bool
             | Character Char

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseChar
        <|> try parseNumber
        <|> try parseFloat
        <|> try parseBool
        <|> parseQuoted
        <|> parseList
        <|> parseBackQuoteList
        <|> parseUnquoteList
            
parseList :: Parser LispVal
parseList = char '(' >>
             ((try parseNormalList) <|> parseDottedList) >>=
             \x -> char ')' >> return x

parseNormalList :: Parser LispVal
parseNormalList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = endBy parseExpr spaces >>=
                  \head -> char '.' >> spaces >> parseExpr >>=
                  \tail -> return $ DottedList head tail

parseBackQuoteList :: Parser LispVal
parseBackQuoteList = char '`' >>
                     parseExpr >>=
                     \x -> return (List [Atom "backquote", x])

parseUnquoteList :: Parser LispVal
parseUnquoteList = char ',' >>
               parseExpr >>=
               \x -> return (List [Atom "unquote", x])

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >>
              parseExpr >>=
              \x -> return $ List [Atom "quote", x]

parseAtom :: Parser LispVal
parseAtom = (letter <|> symbol) >>=
            \first -> (many (letter <|> digit <|> symbol)) >>=
            \rest -> let atom = [first] ++ rest in
            return $ Atom atom

parseString :: Parser LispVal
parseString = char '"' >>
              (many $ many1 (noneOf "\"\\") <|> escapeSequence) >>=
              \x -> (char '"') >> (return $ String $ concat x)

parseChar :: Parser LispVal
parseChar = string "#\\" >> (special <|> anyChar) >>=
            \x -> return $ Character x

parseNumber :: Parser LispVal
parseNumber = parseFloat <|> parseRational <|> parseHex <|> parseDecimal <|> parseOct <|> parseBinary <|> parseComplex

parseBool :: Parser LispVal
parseBool = char '#' >>
            oneOf "tf" >>=
            \boolean -> return $ case boolean of
            't' -> Bool True
            'f' -> Bool False

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

escapeSequence :: Parser String
escapeSequence = char '\\' >>
                    oneOf escapeSequences >>= \sequence ->
                    case sequence of
                        '\\' -> return [sequence]
                        '"' -> return [sequence]
                        'n' -> return "\n"
                        't' -> return "\t"
                        'r' -> return "\r"
                        'b' -> return "\b"

escapeSequences :: [Char]
escapeSequences = ['\\', '"', '\n', '\r', '\t', '\b']

special = (string "space" <|> string "tab" <|> string "linefeed" <|> string "return") >>= \x -> case x of
          "space" -> return ' '
          "tab" ->  return '\t'
          "linefeed" -> return '\n'
          "return" -> return '\r'

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

parseFloat :: Parser LispVal
parseFloat = many1 digit >>=
             \whole -> char '.' >> many1 digit >>=
             \fractional -> let floatingPointNumber = whole ++ "." ++ fractional in 
                                return $ Float (convert readFloat floatingPointNumber)
             
parseRational :: Parser LispVal
parseRational = many1 digit >>=
                \numerator -> char '/' >> many1 digit >>=
                \denominator -> return $ Rational (read numerator) (read denominator)

parseComplex :: Parser LispVal
parseComplex = parseFloat >>=
               \realPart -> oneOf "+-" >>
               parseFloat >>=
               \imaginaryPart -> char 'i' >>
               return (Complex (extractFloat realPart) (extractFloat imaginaryPart))

convert :: (Eq a, Num a, Read a) => ReadS a -> String -> a
convert base num = fst $ base num !! 0

convertBinary :: String -> Integer
convertBinary = convertBinaryAuxiliary 0

convertBinaryAuxiliary :: Integer -> String -> Integer
convertBinaryAuxiliary digInt "" = digInt

convertBinaryAuxiliary digInt (x:xs) = let old = 2*digInt + (read [x])::Integer in
                                       convertBinaryAuxiliary old xs

extractFloat :: LispVal -> Double 
extractFloat (Float value) = value
