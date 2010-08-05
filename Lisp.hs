module Lisp (LispVal(..)
            ,printLisp
            ,parseLisp
            )
where
import Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = LispInteger Integer
             deriving (Show, Eq)

printLisp :: LispVal -> String
printLisp (LispInteger x) = show x

parseInteger :: Parser LispVal
parseInteger = fmap (LispInteger . read) $ many1 digit

parseVal :: Parser LispVal
parseVal = parseInteger

parseValsAndEof = do
  skipMany space
  xs <- sepBy1 parseVal spaces
  eof
  return xs

parseLisp :: [Char] -> Either ParseError [LispVal]
parseLisp s = parse parseValsAndEof "" s
