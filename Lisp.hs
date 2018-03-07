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

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Lisp (Sexp, toSexp, fromSexp, printSexp, LispVal(..), mapcar', mapcarUpToPlist, readLisp, readLisp', cons,
             clNull, car, cdr, getf, getf', atom, fromLispList, parseLisp, printLisp, propertyListP,
             lispEscapeString, nil, n60
            ,lispToRational)
where
import Types (WRat)
import Text.ParserCombinators.Parsec
import Data.Char (toUpper)
import Data.List (elemIndex)
import Data.Maybe
import Data.Ratio (numerator, denominator)

data LispVal = LispInteger Integer
             | LispKeyword String
             | LispSymbol String
             | LispString String
             | LispFloat Rational
             | LispRatio Rational
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
printLisp (LispFloat x) = show x'
  where x' = fromRational x
        x' :: Double
printLisp (LispRatio r) = show (numerator r) ++ "/" ++ show (denominator r)
printLisp (LispKeyword x) = ':' : x
printLisp (LispSymbol x) = x
printLisp (LispString x) = lispEscapeString x
printLisp (LispList xs) =
    "(" ++ unwords (map printLisp xs) ++ ")"

comment :: Parser ()
comment = char ';' >> many (noneOf "\n") >> return ()

whitespace :: Parser ()
whitespace = skipMany (skipMany1 space <|> comment)

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
      let su = map toUpper s
      return (case su of
               "NIL" -> LispList []
               _     -> LispSymbol su)

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
      _ <- char '.'
      ds2 <- many1 digit
      let ds' = read ds :: Integer
      let ds2' = read ds2 :: Integer
      let n = fromInteger ds'
      let numerator = fromInteger ds2'
      let denominator = 10^^(fromIntegral (length ds2) :: Integer)
      let r = (n + (numerator / denominator)) :: Rational
      (LispInteger e) <- (oneOf "sfdleSFDLE" >> parseInteger) <|>
                         return (LispInteger 0)
      return $ (LispFloat . (*10^^e) . (*sign)) r

parseRatio :: Parser LispVal
parseRatio =
    do
      sign <- (char '-' >> return (-1)) <|>
              return 1
      numerator <- many1 digit
      _ <- char '/'
      denominator <- many1 digit
      let numerator' = read numerator :: Integer
      let denominator' = read denominator :: Integer
      return $ (LispRatio . (*sign)) (fromIntegral numerator' / fromIntegral denominator')

parseList :: Parser LispVal
parseList =
    do
      char '(' >> whitespace
      elts <- endBy parseVal whitespace
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
  whitespace
  xs <- endBy1 parseVal whitespace
  eof
  return xs

-- | Parse a String to either a list of LispVals (the string can
-- | contain more than one form), or to ParseError.
parseLisp :: String -> Either String [LispVal]
parseLisp s = case parse parseValsAndEof "" s of
  Right x -> Right x
  Left err -> Left $ "parse error " ++ show err

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
                               let xs' = fromLispList xs
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

instance Sexp [LispVal] where
  toSexp = LispList
  fromSexp (LispList xs) = xs
  fromSexp _ = error "fromSexp: not (LispList xs)"

n60 :: LispVal
n60 = readLisp' "(60)"
nil :: LispVal
nil = readLisp' "()"

lispToRational :: LispVal -> WRat
lispToRational (LispInteger x) = fromInteger x
lispToRational (LispRatio x) = x
lispToRational (LispFloat x) = x
lispToRational x = error $ "cannot convert to rational " ++ show x

