module Lisp (LispVal(..)
            ,printLisp
            ,parseLisp
            )
where
import Text.ParserCombinators.Parsec
import Data.Char (toUpper)
import Data.List (intercalate)

data LispVal = LispInteger Integer
             | LispKeyword String
             | LispFloat Float
             | LispList [LispVal]
             deriving (Show, Eq)

-- |Print a LispVal to String.
printLisp :: LispVal -> String
printLisp (LispInteger x) = show x
printLisp (LispFloat x) = show x
printLisp (LispKeyword x) = ":" ++ x
printLisp (LispList xs) =
    "(" ++ (intercalate " " (map printLisp xs)) ++ ")"

symbol = oneOf "/!$%"

parseKeyword :: Parser LispVal
parseKeyword =
    do
      char ':'
      s <- many1 (letter <|> symbol)
      return (LispKeyword (map toUpper s))

parseInteger :: Parser LispVal
parseInteger =
    do
      sign <- (char '-' >> return (-1)) <|>
              return 1
      ds <- many1 digit
      return $ (LispInteger . (*sign) . read) ds

parseFloat :: Parser LispVal
parseFloat =
    do
      sign <- (char '-' >> return (-1)) <|>
              return 1
      ds <- many1 digit
      dot <- char '.'
      ds2 <- many1 digit
      return $ (LispFloat . (*sign) . read) (ds ++ [dot] ++ ds2)

parseList :: Parser LispVal
parseList =
    do
      char '('
      elts <- sepBy parseVal spaces
      char ')'
      return $ LispList elts

parseVal :: Parser LispVal
parseVal = parseKeyword <|>
           parseList <|>
           (try parseFloat) <|>
           parseInteger

parseValsAndEof = do
  skipMany space
  xs <- endBy1 parseVal spaces
  eof
  return xs

-- |Parse a String to either a list of LispVals (the string can
-- |contain more than one form), or to ParseError.
parseLisp :: [Char] -> Either ParseError [LispVal]
parseLisp s = parse parseValsAndEof "" s