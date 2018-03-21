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

module LispTest

where

import qualified Lisp as L ( LispVal(..)
                           , readLisp
                           , mapcar'
                           , toSexp
                           , printLisp
                           , parseLisp
                           , lispEscapeString
                           , propertyListP
                           , getf)

import Test.HUnit ( Assertion, Test(TestList), (@=?), (~=?) )
import Data.Maybe (fromMaybe)
import Data.Either.Unwrap (fromLeft, fromRight)
import Data.Ratio ( (%) )

rightOrError :: Either t t1 -> t1
rightOrError x = case x of
                   Right y -> y
                   Left _ -> error "rightOrError"

readLisp1 :: Assertion
readLisp1 = Right (L.LispInteger 1) @=? L.readLisp "1"

readLisp2 :: Assertion
readLisp2 = "readLisp: expecting only a single form" @=? fromLeft (L.readLisp "1 2")

readLisp3 :: Assertion
readLisp3 = "readLisp: cannot parse '('" @=? fromLeft (L.readLisp "(")

prop_mapcar' :: [Integer] -> Bool
prop_mapcar' xs = L.mapcar' f (L.LispList (map L.toSexp xs)) == map g xs
  where f (L.LispInteger x) = L.LispInteger $ x+1
        f _ = error "prop_mapcar'"
        g = L.toSexp . (+1)

prop_string_roundTrip :: String -> Bool
prop_string_roundTrip s = (L.printLisp . (!!0) . fromRight . L.parseLisp . p) s == p s
  where p = L.lispEscapeString

lisp1 :: Test
lisp1 = TestList
        [L.LispInteger 1     ~=? L.LispInteger 1
        ,"123"               ~=? L.printLisp (L.LispInteger 123)
        ,[L.LispInteger 123] ~=? rightOrError (L.parseLisp "123")
        ,[L.LispInteger 123] ~=? rightOrError (L.parseLisp " 123")
        ,"left"              ~=? case L.parseLisp "123(" of
                                   Left  _ -> "left"
                                   Right _ -> "right"
        ,[L.LispInteger 1,
          L.LispInteger 2]   ~=? rightOrError (L.parseLisp "1 2")
        ]

lisp2 :: Test
lisp2 = TestList
        [
         [L.LispFloat 123.12] ~=? rightOrError (L.parseLisp "123.12")
        ,[L.LispFloat 123.12] ~=? rightOrError (L.parseLisp "123.12d0")
        ,[L.LispFloat 12.312] ~=? rightOrError (L.parseLisp "123.12D-1")
        ,[L.LispKeyword "FOO"] ~=? rightOrError (L.parseLisp ":FOO")
        ,[L.LispKeyword "BAR"] ~=? rightOrError (L.parseLisp ":bar")
        ,[L.LispKeyword "F/123"] ~=? rightOrError (L.parseLisp ":f/123")
        ,[L.LispList [L.LispInteger 1]] ~=?
         rightOrError (L.parseLisp "(1)")
        ,[L.LispList [L.LispInteger 1]] ~=?
         rightOrError (L.parseLisp "( 1)")
        ,[L.LispList [L.LispInteger 1]] ~=?
         rightOrError (L.parseLisp "(\n1)")
        ,[L.LispList [L.LispInteger 1]] ~=?
         rightOrError (L.parseLisp "( \n1)")
        ,[L.LispList [L.LispInteger 1]] ~=?
         rightOrError (L.parseLisp "(1 )")
        ,[L.LispList [L.LispInteger 1, L.LispInteger 2]] ~=?
         rightOrError (L.parseLisp "(1 2)")
        ,[L.LispList []] ~=?
         rightOrError (L.parseLisp "()")
        ,[L.LispList []] ~=?
         rightOrError (L.parseLisp "nil")
        ,[L.LispList []] ~=?
         rightOrError (L.parseLisp "NIL")
        ,[L.LispList [L.LispList [L.LispInteger 1]]] ~=?
         rightOrError (L.parseLisp "((1))")
        ,[L.LispInteger (-123)] ~=?
         rightOrError (L.parseLisp "-123")
        ,[L.LispFloat (-123.12)] ~=?
         rightOrError (L.parseLisp "-123.12")
        ,[L.LispInteger 1] ~=?
         rightOrError (L.parseLisp "1 ")
        ,[L.LispInteger 1] ~=?
         rightOrError (L.parseLisp "+1 ")
        ,[L.LispRatio $ 1 % 2] ~=?
         rightOrError (L.parseLisp "1/2")
        ,[L.LispRatio $ (-1) % 10] ~=?
         rightOrError (L.parseLisp "-1/10")
        ,[L.LispSymbol "T"] ~=?
         rightOrError (L.parseLisp "t")
        ]

parseComment1 :: Test
parseComment1 = TestList
                [
                  [L.LispInteger 25] ~=? rightOrError (L.parseLisp "25 ;foo")
                , [L.LispInteger 25] ~=? rightOrError (L.parseLisp "25;foo")
                , [L.LispInteger 25] ~=? rightOrError (L.parseLisp "25\n;foo\n;foo\n")
                , [L.LispInteger 25] ~=? rightOrError (L.parseLisp ";foo\n25")
                , [L.LispList [L.LispInteger 1, L.LispInteger 2]] ~=?
                    rightOrError (L.parseLisp "(1 ;foo\n2)")
                , [L.LispInteger 25] ~=? rightOrError (L.parseLisp "25 ;bar")
                , [L.LispInteger 25] ~=? rightOrError (L.parseLisp "25;;;")
                , [L.LispInteger 25] ~=? rightOrError (L.parseLisp "25;")
                ]

lisp3 :: Test
lisp3 = TestList
        [
         "(:FDS 1 2.3)" ~=?
         L.printLisp (L.LispList [L.LispKeyword "FDS",L.LispInteger 1,L.LispFloat 2.3])
        ,"()" ~=?
         L.printLisp (L.LispList [])
        ,"T" ~=?
         L.printLisp (L.LispSymbol "T")
        ,True ~=?
         L.propertyListP (L.LispList [L.LispKeyword "FDS",L.LispInteger 1])
        ,False ~=?
         L.propertyListP (L.LispList [L.LispKeyword "FDS"])
        ,False ~=?
         L.propertyListP (L.LispList [L.LispInteger 1, L.LispInteger 2])
        ,False ~=?
         L.propertyListP (L.LispInteger 1)
        ,L.LispInteger 1 ~=?
         fromMaybe (error "Nothing") (L.getf (L.LispList [L.LispKeyword "FDS",L.LispInteger 1]) (L.LispKeyword "FDS"))
        ,Nothing ~=?
         L.getf (L.LispList [L.LispKeyword "FDS",L.LispInteger 1]) (L.LispKeyword "BAR")
        ]
