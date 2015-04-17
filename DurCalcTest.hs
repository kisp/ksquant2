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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DurCalcTest where

import qualified Test.Framework.Providers.API as TF
import Test.Framework.Providers.API(testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.Framework.Providers.QuickCheck2(testProperty)
import Test.HUnit
import Test.QuickCheck

import Data.Ratio
import DurCalc
import Control.Monad

newtype Dur = Dur Rational
    deriving (Show, Eq)

-- instance Arbitrary Dur where
--     arbitrary = do
--       n <- choose (-32,32)
--       d <- choose (1,32)
--       return (n % d)

instance Arbitrary Dur where
    arbitrary = liftM Dur ratio
        where ratio = (do
                n <- choose (-32,32)
                d <- choose (1,32)
                return (n%d)) :: Gen Rational

notableDur01 :: Assertion
notableDur01 = True @=? notableDur 0 (1%4)

notableDur02 :: Assertion
notableDur02 = False @=? notableDur 3 (5%4)

prop_notableDur_abs :: Integer -> Dur -> Property
prop_notableDur_abs m (Dur r) = m >= 0 ==> notableDur m r == notableDur m (abs r)

prop_notableDur_half :: Integer -> Dur -> Property
prop_notableDur_half m (Dur r) = m >= 0 ==> notableDur m r == notableDur m (r / 2)

prop_notableDur_dot1 :: Integer -> Bool
prop_notableDur_dot1 n = notableDur 1 (r * dotFactor 1)
    where r = exp2 n

prop_notableDur_dot2 :: Integer -> Bool
prop_notableDur_dot2 n = notableDur 2 (r * dotFactor 2)
    where r = exp2 n

prop_notableDur_dot3 :: Integer -> Bool
prop_notableDur_dot3 n = notableDur 3 (r * dotFactor 3)
    where r = exp2 n

prop_notableDur_dot4 :: Integer -> Bool
prop_notableDur_dot4 n = notableDur 4 (r * dotFactor 4)
    where r = exp2 n

dotFactor0 :: Assertion
dotFactor0 = 1 @=? dotFactor 0

dotFactor1 :: Assertion
dotFactor1 = (3%2) @=? dotFactor 1

dotFactor2 :: Assertion
dotFactor2 = (7%4) @=? dotFactor 2

prop_dotFactor_geq_one :: Integer -> Property
prop_dotFactor_geq_one n = n >= 0 ==> dotFactor n >= 1

prop_dotFactor_lt_two :: Integer -> Property
prop_dotFactor_lt_two n = n >= 0 ==> dotFactor n < 2

prop_dotFactor_monotone :: Integer -> Property
prop_dotFactor_monotone n = n >= 0 ==> dotFactor n < dotFactor (n+1)

exp2_0 :: Assertion
exp2_0 = 1 @=? exp2 0

exp2_1 :: Assertion
exp2_1 = 2 @=? exp2 1

exp2_2 :: Assertion
exp2_2 = (1%2) @=? exp2 (-1)

prop_exp2_double :: Integer -> Bool
prop_exp2_double n = 2 * exp2 n == exp2 (n+1)

isExp2_0 :: Assertion
isExp2_0 = True @=? isExp2 1

isExp2_1 :: Assertion
isExp2_1 = True @=? isExp2 8

isExp2_2 :: Assertion
isExp2_2 = True @=? isExp2 (1%2)

isExp2_3 :: Assertion
isExp2_3 = False @=? isExp2 3

isExp2_4 :: Assertion
isExp2_4 = False @=? isExp2 7

isExp2_5 :: Assertion
isExp2_5 = False @=? isExp2 (1%3)

isExp2_6 :: Assertion
isExp2_6 = False @=? isExp2 (4%3)

isExp2_7 :: Assertion
isExp2_7 = False @=? isExp2 (3%4)

isExp2_8 :: Assertion
isExp2_8 = False @=? isExp2 0

prop_isExp2_complete :: Integer -> Bool
prop_isExp2_complete n = isExp2 (exp2 n)

splitNotable1 :: Assertion
splitNotable1 = Just [1%8] @=? splitNotable 3 (1%8)

durCalcTests :: TF.Test
durCalcTests = testGroup "durCalcTests"
                  [
                    testCase "notableDur01" notableDur01
                  , testCase "notableDur02" notableDur02
                  , testProperty "prop_notableDur_abs" prop_notableDur_abs
                  , testProperty "prop_notableDur_half" prop_notableDur_half
                  , testProperty "prop_notableDur_dot1" prop_notableDur_dot1
                  , testProperty "prop_notableDur_dot2" prop_notableDur_dot2
                  , testProperty "prop_notableDur_dot3" prop_notableDur_dot3
                  , testProperty "prop_notableDur_dot4" prop_notableDur_dot4
                  , testCase "dotFactor0" dotFactor0
                  , testCase "dotFactor1" dotFactor1
                  , testCase "dotFactor2" dotFactor2
                  , testProperty "prop_dotFactor_geq_one" prop_dotFactor_geq_one
                  , testProperty "prop_dotFactor_lt_two" prop_dotFactor_lt_two
                  , testProperty "prop_dotFactor_monotone" prop_dotFactor_monotone
                  , testCase "exp2_0" exp2_0
                  , testCase "exp2_1" exp2_1
                  , testCase "exp2_2" exp2_2
                  , testProperty "prop_exp2_double" prop_exp2_double
                  , testCase "isExp2_0" isExp2_0
                  , testCase "isExp2_1" isExp2_1
                  , testCase "isExp2_2" isExp2_2
                  , testCase "isExp2_3" isExp2_3
                  , testCase "isExp2_4" isExp2_4
                  , testCase "isExp2_5" isExp2_5
                  , testCase "isExp2_6" isExp2_6
                  , testCase "isExp2_7" isExp2_7
                  , testCase "isExp2_8" isExp2_8
                  , testProperty "prop_isExp2_complete" prop_isExp2_complete
                  , testCase "splitNotable1" splitNotable1
                  ]
