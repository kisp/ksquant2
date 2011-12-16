-- This file is part of KSQuant2.

-- Copyright (c) 2010 - 2011, Kilian Sprotte. All rights reserved.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Lisp (Sexp, toSexp, fromSexp, printSexp, LispVal(..), mapcar', mapcarUpToPlist, readLisp, readLisp', cons,
             clNull, car, cdr, getf, getf', atom, fromLispList, parseLisp, printLisp, propertyListP,
             lispEscapeString)
where
import Text.ParserCombinators.Parsec
import Data.Char (toUpper)
import Data.List (elemIndex)
import Data.Maybe

data LispVal = LispInteger Integer
             | LispKeyword String
             | LispSymbol String
             | LispString String
             | LispFloat Float
             | LispList [LispVal]
             deriving (Eq)

instance Show LispVal where
    show x = "readLisp \"" ++ printLisp x ++ "\""

lispEscapeString :: String -> String
lispEscapeString s = "\"" ++ rec s ++ "\""
  where
    rec [] = []
    rec (x:xs) | x == '\"' = "\\\"" ++ rec xs
               | x == '\\' = "\\\\" ++ rec xs
               | otherwise = x : rec xs

-- |Print a LispVal to String.
printLisp :: LispVal -> String
printLisp (LispInteger x) = show x
printLisp (LispFloat x) = show x
printLisp (LispKeyword x) = ':' : x
printLisp (LispSymbol x) = x
printLisp (LispString x) = lispEscapeString x
printLisp (LispList xs) =
    "(" ++ unwords (map printLisp xs) ++ ")"

symbol :: Parser Char
symbol = oneOf "/!$%-"

parseKeyword :: Parser LispVal
parseKeyword =
    do
      _ <- char ':'
      s <- many1 (letter <|> symbol <|> digit)
      return (LispKeyword (map toUpper s))

parseSymbol :: Parser LispVal
parseSymbol =
    do
      s <- many1 (letter <|> symbol <|> digit)
      return (LispSymbol (map toUpper s))

parseString :: Parser LispVal
parseString =
    do
      _ <- char '\"'
      s <- many ((char '\\' >> anyToken) <|> noneOf "\"")
      _ <- char '\"'
      return (LispString s)

parseSign :: (Num a) => Parser a
parseSign = (char '-' >> return (-1)) <|>
            (char '+' >> return 1) <|>
            return 1

parseInteger :: Parser LispVal
parseInteger =
    do
      sign <- parseSign
      ds <- many1 digit
      return $ (LispInteger . (*sign) . read) ds

parseFloat :: Parser LispVal
parseFloat =
    do
      sign <- parseSign
      ds <- many1 digit
      dot <- char '.'
      ds2 <- many1 digit
      (LispInteger e) <- (oneOf "sfdleSFDLE" >> parseInteger) <|>
                         return (LispInteger 0)
      return $ (LispFloat . (*10^^e) . (*sign) . read) (ds ++ [dot] ++ ds2)

parseRatio :: Parser LispVal
parseRatio =
    do
      sign <- (char '-' >> return (-1)) <|>
              return 1
      numerator <- many1 digit
      _ <- char '/'
      denominator <- many1 digit
      let numerator' = read numerator
      let denominator' = read denominator
      return $ (LispFloat . (*sign)) (numerator' / denominator')

parseList :: Parser LispVal
parseList =
    do
      _ <- char '('
      elts <- sepBy parseVal spaces
      _ <- char ')'
      return $ LispList elts

parseVal :: Parser LispVal
parseVal = parseKeyword <|>
           parseList <|>
           parseString <|>
           try parseFloat <|>
           try parseRatio <|>
           parseInteger <|>
           parseSymbol

parseValsAndEof :: Parser [LispVal]
parseValsAndEof = do
  skipMany space
  xs <- endBy1 parseVal spaces
  eof
  return xs

-- |Parse a String to either a list of LispVals (the string can
-- |contain more than one form), or to ParseError.
parseLisp :: String -> Either ParseError [LispVal]
parseLisp = parse parseValsAndEof ""

readLisp :: String -> Either String LispVal
readLisp s = case parseLisp s of
                 Right [x] -> Right x
                 Right _   -> Left "readLisp: expecting only a single form"
                 Left _    -> Left $ "readLisp: cannot parse '" ++ s ++ "'"


readLisp' :: String -> LispVal
readLisp' s = case readLisp s of
  Right x -> x
  Left x -> error x

----------------------------------------------

listp :: LispVal -> Bool
listp (LispList _) = True
listp _ = False

atom :: LispVal -> Bool
atom = not . listp

clNull :: LispVal -> Bool
clNull (LispSymbol "NIL") = True
clNull (LispList []) = True
clNull _ = False

cons :: LispVal -> LispVal -> LispVal
cons x (LispList ys) = LispList (x:ys)
cons x y             =
    error ("cons `" ++ show x ++ "' to `" ++ show y ++ "'")

car :: LispVal -> LispVal
car (LispList (x:_)) = x
car x                = error $ "car on '" ++ show x ++ "'"

cdr :: LispVal -> LispVal
cdr (LispList (_:xs)) = LispList xs
cdr x                = error $ "cdr on '" ++ show x ++ "'"

fromLispList :: LispVal -> [LispVal]
fromLispList (LispList xs) = xs
fromLispList _ = error "fromLispList: not a list"

-- mapcar :: (LispVal -> LispVal) -> LispVal -> LispVal
-- mapcar _ (LispList []) = LispList []
-- mapcar f xs@(LispList _) = f a `cons` mapcar f b
--     where a = car xs
--           b = cdr xs
-- mapcar _ _ = error "mapcar: not a list"

mapcar' :: (LispVal -> a) -> LispVal -> [a]
mapcar' _ (LispList []) = []
mapcar' f xs@(LispList _) = f a : mapcar' f b
    where a = car xs
          b = cdr xs
mapcar' _ _ = error "mapcar': not a list"

mapcarUpToPlist :: (LispVal -> a) -> LispVal -> ([a], LispVal)
mapcarUpToPlist _ (LispList []) = ([], LispList [])
mapcarUpToPlist _ xs@(LispList _) | propertyListP xs = ([], xs)
mapcarUpToPlist f xs@(LispList _) = (f a : ys, plist)
    where a = car xs
          b = cdr xs
          (ys,plist) = mapcarUpToPlist f b
mapcarUpToPlist _ _ = error "mapcarUpToPlist: not a list"

keywordp :: LispVal -> Bool
keywordp (LispKeyword _) = True
keywordp _ = False

propertyListP :: LispVal -> Bool
propertyListP (LispList xs) = (even . length) xs &&
                              all keywordp (everySecond xs)
    where everySecond [] = []
          everySecond (a:_:ys) = a : everySecond ys
          everySecond [_] = error "propertyListP: is this really a plist?"

propertyListP _ = False

getf :: LispVal -> LispVal -> Maybe LispVal
getf xs@(LispList _) field | propertyListP xs =
                               let xs' = (fromLispList xs)
                               in do
                                 index <- elemIndex field xs'
                                 return (xs'!!(index+1))
getf x y =
    error ("getf `" ++ show x ++ "' to `" ++ show y ++ "'")

-- | getf with default value
getf' :: LispVal -> LispVal -> LispVal -> LispVal
getf' list field def = fromMaybe def (getf list field)

-- minus :: LispVal -> LispVal
-- minus (LispInteger x) = LispInteger (-x)
-- minus (LispFloat x) = LispFloat (-x)
-- minus _ = error "minus"

----------------------------------------------------

class Sexp a where
    toSexp :: a -> LispVal
    fromSexp :: LispVal -> a
    printSexp :: a -> String
    printSexp = printLisp . toSexp

instance Sexp LispVal where
  toSexp = id
  fromSexp = id

instance Sexp Integer where
  toSexp = LispInteger
  fromSexp (LispInteger x) = x
  fromSexp _ = error "fromSexp: not (LispInteger x)"
