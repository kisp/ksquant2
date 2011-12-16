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

module Test where

import Test.Runner

import IntervalTest
import LispTest
import MeasureTest
import MeasureToEnpTest
import SimpleFormatTest

-- -- use HUnit to assert that helloWorld produces "hello world"
-- hunitTest :: Test
-- hunitTest = TestCase $ do
--     assertEqual "hello world" "hello world" helloWorld

-- -- use QuickCheck to check the length of hello's result
-- helloLength :: String -> Bool
-- helloLength s = length (hello s) == length "hello " + length s

-- -- A simple boolean expression that states that hello of an empty string is
-- -- "hello"
-- helloEmpty :: Bool
-- helloEmpty = hello "" == "hello "

tests :: [(String, TestRunnerTest)]
tests = [-- ("helloWorld value", TestRunnerTest hunitTest),
         -- ("hello length", runWithQuickCheck helloLength),
         -- ("value of hello applied to empty string", TestRunnerTest helloEmpty)
         ("prop_good_iv", runWithQuickCheck prop_good_iv)
         ,("prop_isPointInInterval", runWithQuickCheck prop_isPointInInterval)
         ,("prop_intersect", runWithQuickCheck prop_intersect)
         ,("prop_isStrictlyAfter", runWithQuickCheck prop_isStrictlyAfter)
         ,("prop_groupPointsByIntervalls", runWithQuickCheck prop_groupPointsByIntervalls)
         ,("prop_ascendingIntervals2points", runWithQuickCheck prop_ascendingIntervals2points)
         -- LispTest
         ,("readLisp1", TestRunnerTest readLisp1)
         ,("prop_mapcar'", runWithQuickCheck prop_mapcar')
         ,("lisp1", TestRunnerTest lisp1)
         ,("lisp2", TestRunnerTest lisp2)
         ,("lisp3", TestRunnerTest lisp3)
         -- Measure Test
         ,("measure1", TestRunnerTest measure1)
         ,("mtoenp1", TestRunnerTest mtoenp1)
         -- SimpleFormatTest
         ,("sexp2event1", TestRunnerTest sexp2event1)
         ]
main :: IO ()
main = testRunnerMain tests
