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

module LispTest where
import Lisp
import Test.HUnit
import Data.Maybe
import Data.Either.Unwrap
import Data.Ratio

rightOrError :: Either t t1 -> t1
rightOrError x = case x of
                   Right y -> y
                   Left _ -> error "rightOrError"

readLisp1 :: Assertion
readLisp1 = Right (LispInteger 1) @=? readLisp "1"

readLisp2 :: Assertion
readLisp2 = "readLisp: expecting only a single form" @=? fromLeft (readLisp "1 2")

readLisp3 :: Assertion
readLisp3 = "readLisp: cannot parse '('" @=? fromLeft (readLisp "(")

prop_mapcar' :: [Integer] -> Bool
prop_mapcar' xs = mapcar' f (LispList (map toSexp xs)) == map g xs
  where f (LispInteger x) = LispInteger $ x+1
        f _ = error "prop_mapcar'"
        g = toSexp . (+1)

prop_string_roundTrip :: String -> Bool
prop_string_roundTrip s = (printLisp . (!!0) . fromRight . parseLisp . p) s == p s
  where p = lispEscapeString

lisp1 :: Test
lisp1 = TestList
        [LispInteger 1     ~=? LispInteger 1
        ,"123"               ~=? printLisp (LispInteger 123)
        ,[LispInteger 123] ~=? rightOrError (parseLisp "123")
        ,[LispInteger 123] ~=? rightOrError (parseLisp " 123")
        ,"left"              ~=? case parseLisp "123(" of
                                   Left  _ -> "left"
                                   Right _ -> "right"
        ,[LispInteger 1,
          LispInteger 2]   ~=? rightOrError (parseLisp "1 2")
        ]

lisp2 :: Test
lisp2 = TestList
        [
         [LispFloat 123.12] ~=? rightOrError (parseLisp "123.12")
        ,[LispFloat 123.12] ~=? rightOrError (parseLisp "123.12d0")
        ,[LispFloat 12.312] ~=? rightOrError (parseLisp "123.12D-1")
        ,[LispKeyword "FOO"] ~=? rightOrError (parseLisp ":FOO")
        ,[LispKeyword "BAR"] ~=? rightOrError (parseLisp ":bar")
        ,[LispKeyword "F/123"] ~=? rightOrError (parseLisp ":f/123")
        ,[LispList [LispInteger 1]] ~=?
         rightOrError (parseLisp "(1)")
        ,[LispList [LispInteger 1]] ~=?
         rightOrError (parseLisp "( 1)")
        ,[LispList [LispInteger 1]] ~=?
         rightOrError (parseLisp "(\n1)")
        ,[LispList [LispInteger 1]] ~=?
         rightOrError (parseLisp "( \n1)")
        ,[LispList [LispInteger 1]] ~=?
         rightOrError (parseLisp "(1 )")
        ,[LispList [LispInteger 1, LispInteger 2]] ~=?
         rightOrError (parseLisp "(1 2)")
        ,[LispList []] ~=?
         rightOrError (parseLisp "()")
        ,[LispList []] ~=?
         rightOrError (parseLisp "nil")
        ,[LispList []] ~=?
         rightOrError (parseLisp "NIL")
        ,[LispList [LispList [LispInteger 1]]] ~=?
         rightOrError (parseLisp "((1))")
        ,[LispInteger (-123)] ~=?
         rightOrError (parseLisp "-123")
        ,[LispFloat (-123.12)] ~=?
         rightOrError (parseLisp "-123.12")
        ,[LispInteger 1] ~=?
         rightOrError (parseLisp "1 ")
        ,[LispInteger 1] ~=?
         rightOrError (parseLisp "+1 ")
        ,[LispRatio $ 1 % 2] ~=?
         rightOrError (parseLisp "1/2")
        ,[LispRatio $ (-1) % 10] ~=?
         rightOrError (parseLisp "-1/10")
        ,[LispSymbol "T"] ~=?
         rightOrError (parseLisp "t")
        ]

parseComment1 :: Test
parseComment1 = TestList
                [
                  [LispInteger 25] ~=? rightOrError (parseLisp "25 ;foo")
                , [LispInteger 25] ~=? rightOrError (parseLisp "25;foo")
                , [LispInteger 25] ~=? rightOrError (parseLisp "25\n;foo\n;foo\n")
                , [LispInteger 25] ~=? rightOrError (parseLisp ";foo\n25")
                , [LispList [LispInteger 1, LispInteger 2]] ~=?
                    rightOrError (parseLisp "(1 ;foo\n2)")
                , [LispInteger 25] ~=? rightOrError (parseLisp "25 ;bar")
                , [LispInteger 25] ~=? rightOrError (parseLisp "25;;;")
                , [LispInteger 25] ~=? rightOrError (parseLisp "25;")
                ]

lisp3 :: Test
lisp3 = TestList
        [
         "(:FDS 1 2.3)" ~=?
         printLisp (LispList [LispKeyword "FDS",LispInteger 1,LispFloat 2.3])
        ,"()" ~=?
         printLisp (LispList [])
        ,"T" ~=?
         printLisp (LispSymbol "T")
        ,True ~=?
         propertyListP (LispList [LispKeyword "FDS",LispInteger 1])
        ,False ~=?
         propertyListP (LispList [LispKeyword "FDS"])
        ,False ~=?
         propertyListP (LispList [LispInteger 1, LispInteger 2])
        ,False ~=?
         propertyListP (LispInteger 1)
        ,LispInteger 1 ~=?
         fromMaybe (error "Nothing") (getf (LispList [LispKeyword "FDS",LispInteger 1]) (LispKeyword "FDS"))
        ,Nothing ~=?
         getf (LispList [LispKeyword "FDS",LispInteger 1]) (LispKeyword "BAR")
        ]
