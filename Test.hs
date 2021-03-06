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

module Test

where

import System.Environment (getArgs)
import Test.Framework ( Test, testGroup, defaultMainWithArgs )
import Test.Framework.Providers.HUnit ( testCase, hUnitTestToTests )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import IntervalTest ( prop_good_iv
                    , prop_isPointInInterval
                    , prop_intersect
                    , prop_isStrictlyAfter
                    , prop_groupPointsByIntervalls
                    , prop_ascendingIntervals2points )

import LispTest (readLisp1
                , readLisp2
                , readLisp3
                , prop_mapcar'
                , prop_string_roundTrip
                , lisp1
                , lisp2
                , lisp3
                , parseComment1)

import MeasureTest ( measure1 )
import MeasureToEnpTest ( mtoenp1 )
import SimpleFormatTest ( sexp2event1 )
import AdjoinTiesTest ( adjoin1, adjoin2, adjoin3 )
import DurCalcTest ( durCalcTests )

intervalTests :: Test
intervalTests = testGroup "intervalTests"
                [
                  testProperty "prop_good_iv" prop_good_iv
                , testProperty "prop_isPointInInterval" prop_isPointInInterval
                , testProperty "prop_intersect" prop_intersect
                , testProperty "prop_isStrictlyAfter" prop_isStrictlyAfter
                , testProperty "prop_groupPointsByIntervalls" prop_groupPointsByIntervalls
                , testProperty "prop_ascendingIntervals2points" prop_ascendingIntervals2points
                ]

lispTests :: Test
lispTests = testGroup "lispTests"
            [
              testCase "readLisp1" readLisp1
            , testCase "readLisp2" readLisp2
            , testCase "readLisp3" readLisp3
            , testProperty "prop_mapcar'" prop_mapcar'
            , testProperty "prop_string_roundTrip" prop_string_roundTrip
            , testGroup "lisp1" $ hUnitTestToTests lisp1
            , testGroup "lisp2" $ hUnitTestToTests lisp2
            , testGroup "lisp3" $ hUnitTestToTests lisp3
            , testGroup "parseComment1" $ hUnitTestToTests parseComment1
            ]

measureTests :: Test
measureTests = testGroup "measureTests"
               [
                 testGroup "measure1" $ hUnitTestToTests measure1
               ]

measureToEnpTests :: Test
measureToEnpTests = testGroup "measureToEnpTests"
               [
                 testGroup "mtoenp1" $ hUnitTestToTests mtoenp1
               ]

simpleFormatTests :: Test
simpleFormatTests = testGroup "simpleFormatTests"
               [
                 testGroup "sexp2event1" $ hUnitTestToTests sexp2event1
               ]

adjoinTiesTests :: Test
adjoinTiesTests = testGroup "adjoinTiesTests"
                  [
                    testCase "adjoin1" adjoin1
                  , testCase "adjoin2" adjoin2
                  , testCase "adjoin3" adjoin3
                  ]

tests :: [Test]
tests = [
         intervalTests
       , lispTests
       , measureTests
       , measureToEnpTests
       , simpleFormatTests
       , adjoinTiesTests
       , durCalcTests
       ]

main :: IO ()
main = do
     args <- getArgs
     let args' = ["-a", "1000"] ++ args
     defaultMainWithArgs tests args'
