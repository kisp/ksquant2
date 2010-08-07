module Lisp (LispVal(..)
            ,printLisp
            ,parseLisp
            ,fromLispList
            ,mapcar
            ,mapcar'
            ,cons
            ,car
            ,cdr
            ,propertyListP
            ,getf
            )
where
import Text.ParserCombinators.Parsec
import Data.Char (toUpper)
import Data.List (intercalate,elemIndex)

data LispVal = LispInteger Integer
             | LispKeyword String
             | LispSymbol String
             | LispFloat Float
             | LispList [LispVal]
             deriving (Show, Eq)

-- |Print a LispVal to String.
printLisp :: LispVal -> String
printLisp (LispInteger x) = show x
printLisp (LispFloat x) = show x
printLisp (LispKeyword x) = ":" ++ x
printLisp (LispSymbol x) = x
printLisp (LispList xs) =
    "(" ++ (intercalate " " (map printLisp xs)) ++ ")"

symbol = oneOf "/!$%"

parseKeyword :: Parser LispVal
parseKeyword =
    do
      char ':'
      s <- many1 (letter <|> symbol <|> digit)
      return (LispKeyword (map toUpper s))

parseSymbol :: Parser LispVal
parseSymbol =
    do
      s <- many1 (letter <|> symbol <|> digit)
      return (LispSymbol (map toUpper s))

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

parseRatio :: Parser LispVal
parseRatio =
    do
      sign <- (char '-' >> return (-1)) <|>
              return 1
      numerator <- many1 digit
      char '/'
      denominator <- many1 digit
      numerator' <- return (read numerator)
      denominator' <- return (read denominator)
      return $ (LispFloat . (*sign)) (numerator' / denominator')

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
           (try parseRatio) <|>
           parseInteger <|>
           parseSymbol

parseValsAndEof = do
  skipMany space
  xs <- endBy1 parseVal spaces
  eof
  return xs

-- |Parse a String to either a list of LispVals (the string can
-- |contain more than one form), or to ParseError.
parseLisp :: [Char] -> Either ParseError [LispVal]
parseLisp s = parse parseValsAndEof "" s

----------------------------------------------

cons :: LispVal -> LispVal -> LispVal
cons x (LispList ys) = LispList (x:ys)
cons x y             =
    error ("cons `" ++ (show x) ++ "' to `" ++ (show y) ++ "'")

car :: LispVal -> LispVal
car (LispList (x:_)) = x

cdr :: LispVal -> LispVal
cdr (LispList (_:xs)) = LispList xs

fromLispList (LispList xs) = xs

mapcar :: (LispVal -> LispVal) -> LispVal -> LispVal
mapcar _ (LispList []) = LispList []
mapcar f xs@(LispList _) = (f a) `cons` (mapcar f b)
    where a = car xs
          b = cdr xs

mapcar' :: (LispVal -> a) -> LispVal -> [a]
mapcar' _ (LispList []) = []
mapcar' f xs@(LispList _) = (f a) : (mapcar' f b)
    where a = car xs
          b = cdr xs

keywordp (LispKeyword _) = True
keywordp _ = False

propertyListP (LispList xs) = (even . length) xs &&
                              all keywordp (everySecond xs)
    where everySecond [] = []
          everySecond (a:b:ys) = a : (everySecond ys) 
propertyListP _ = False

getf :: LispVal -> LispVal -> Maybe LispVal
getf xs@(LispList _) field | propertyListP xs =
                               let xs' = (fromLispList xs)
                               in do
                                 index <- elemIndex field xs'
                                 return (xs'!!(index+1))
getf x y =
    error ("getf `" ++ (show x) ++ "' to `" ++ (show y) ++ "'")
