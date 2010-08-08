module Main where

import Test.HUnit
import Test.QuickCheck
import Test.Runner

import IntervalTest
import LispTest
import MeasureTest
import MeasureToEnpTest

import Input
import Interval
import Lily
import Measure
import MeasureToLily
import Utils

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
         ,("prop_ascending_intervals2points", runWithQuickCheck prop_ascending_intervals2points)
         -- LispTest
         ,("lisp1", TestRunnerTest lisp1)
         ,("lisp2", TestRunnerTest lisp2)
         ,("lisp3", TestRunnerTest lisp3)
         -- Measure Test
         ,("measure1", TestRunnerTest measure1)
         ,("mtoenp1", TestRunnerTest mtoenp1)
         ]
main :: IO ()
main = testRunnerMain tests
