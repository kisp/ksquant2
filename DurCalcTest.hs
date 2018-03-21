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



module DurCalcTest

where

import qualified Types as T ( WRat )
import qualified Test.Framework.Providers.API as TF ( Test )
import Test.Framework.Providers.API ( testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.HUnit ( Assertion, (@=?) )
import Test.QuickCheck ( Arbitrary, arbitrary, Gen, Property, choose, (==>) )

import Data.Ratio ( (%) )
import qualified DurCalc as DC ( notableDur, dotFactor, exp2, isExp2 )

newtype Dur = Dur T.WRat
    deriving (Show, Eq)

-- instance Arbitrary Dur where
--     arbitrary = do
--       n <- choose (-32,32)
--       d <- choose (1,32)
--       return (n % d)

instance Arbitrary Dur where
    arbitrary = fmap Dur ratio
        where ratio = (do
                n <- choose (-32,32)
                d <- choose (1,32)
                return (n%d)) :: Gen Rational

notableDur01 :: Assertion
notableDur01 = True @=? DC.notableDur 0 (1%4)

notableDur02 :: Assertion
notableDur02 = False @=? DC.notableDur 3 (5%4)

prop_notableDur_abs :: Integer -> Dur -> Property
prop_notableDur_abs m (Dur r) = m >= 0 ==> DC.notableDur m r == DC.notableDur m (abs r)

prop_notableDur_half :: Integer -> Dur -> Property
prop_notableDur_half m (Dur r) = m >= 0 ==> DC.notableDur m r == DC.notableDur m (r / 2)

prop_notableDur_dot1 :: Integer -> Bool
prop_notableDur_dot1 n = DC.notableDur 1 (r * DC.dotFactor 1)
    where r = DC.exp2 n

prop_notableDur_dot2 :: Integer -> Bool
prop_notableDur_dot2 n = DC.notableDur 2 (r * DC.dotFactor 2)
    where r = DC.exp2 n

prop_notableDur_dot3 :: Integer -> Bool
prop_notableDur_dot3 n = DC.notableDur 3 (r * DC.dotFactor 3)
    where r = DC.exp2 n

prop_notableDur_dot4 :: Integer -> Bool
prop_notableDur_dot4 n = DC.notableDur 4 (r * DC.dotFactor 4)
    where r = DC.exp2 n

dotFactor0 :: Assertion
dotFactor0 = 1 @=? DC.dotFactor 0

dotFactor1 :: Assertion
dotFactor1 = (3%2) @=? DC.dotFactor 1

dotFactor2 :: Assertion
dotFactor2 = (7%4) @=? DC.dotFactor 2

prop_dotFactor_geq_one :: Integer -> Property
prop_dotFactor_geq_one n = n >= 0 ==> DC.dotFactor n >= 1

prop_dotFactor_lt_two :: Integer -> Property
prop_dotFactor_lt_two n = n >= 0 ==> DC.dotFactor n < 2

prop_dotFactor_monotone :: Integer -> Property
prop_dotFactor_monotone n = n >= 0 ==> DC.dotFactor n < DC.dotFactor (n+1)

exp2_0 :: Assertion
exp2_0 = 1 @=? DC.exp2 0

exp2_1 :: Assertion
exp2_1 = 2 @=? DC.exp2 1

exp2_2 :: Assertion
exp2_2 = (1%2) @=? DC.exp2 (-1)

prop_exp2_double :: Integer -> Bool
prop_exp2_double n = 2 * DC.exp2 n == DC.exp2 (n+1)

isExp2_0 :: Assertion
isExp2_0 = True @=? DC.isExp2 1

isExp2_1 :: Assertion
isExp2_1 = True @=? DC.isExp2 8

isExp2_2 :: Assertion
isExp2_2 = True @=? DC.isExp2 (1%2)

isExp2_3 :: Assertion
isExp2_3 = False @=? DC.isExp2 3

isExp2_4 :: Assertion
isExp2_4 = False @=? DC.isExp2 7

isExp2_5 :: Assertion
isExp2_5 = False @=? DC.isExp2 (1%3)

isExp2_6 :: Assertion
isExp2_6 = False @=? DC.isExp2 (4%3)

isExp2_7 :: Assertion
isExp2_7 = False @=? DC.isExp2 (3%4)

isExp2_8 :: Assertion
isExp2_8 = False @=? DC.isExp2 0

prop_isExp2_complete :: Integer -> Bool
prop_isExp2_complete n = DC.isExp2 (DC.exp2 n)

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
                  ]
