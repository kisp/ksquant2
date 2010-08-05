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

parseValAndEof = do
  x <- parseVal
  eof
  return x

parseLisp s = parse parseValAndEof "" s
