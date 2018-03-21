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

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}

module IntervalTest

where

import qualified Interval as Iv ( Interval
                                , start
                                , end
                                , AscendingIntervals
                                , AscendingPoints
                                , isPointInInterval
                                , intersect
                                , isStrictlyAfter
                                , ascendingIntervals
                                , ascendingPoints
                                , groupPointsByIntervalls
                                , getAscendingIntervals
                                , getAscendingPoints
                                , ascendingIntervals2points)

import Test.QuickCheck (Arbitrary, arbitrary, Gen, choose)
import Data.List (delete, sort, nub)

ivmax :: Int
ivmax = 30::Int
domain :: [Int]
domain = [0..ivmax]

data TestInterval = TestInterval (Int,Int)
                    deriving Show

instance Iv.Interval TestInterval Int where
    start (TestInterval x) = Iv.start x
    end (TestInterval x) = Iv.end x

instance Arbitrary TestInterval where
    arbitrary     = do
      a <- choose (0, ivmax-1)
      b <- choose (a+1, ivmax)
      return (TestInterval (a,b))

prop_good_iv :: TestInterval -> Bool
prop_good_iv iv = Iv.start iv < Iv.end iv

prop_isPointInInterval :: TestInterval -> Bool
prop_isPointInInterval iv = any (Iv.isPointInInterval iv) domain

prop_intersect :: TestInterval -> TestInterval -> Bool
prop_intersect a b = Iv.intersect a b == safe_intersect a b
    where safe_intersect _ _ = any inBoth domain
          inBoth x = Iv.isPointInInterval a x && Iv.isPointInInterval b x

prop_isStrictlyAfter :: TestInterval -> TestInterval -> Bool
prop_isStrictlyAfter a b = Iv.isStrictlyAfter a b == safeisStrictlyAfter a b
    where safeisStrictlyAfter a b = not (a `Iv.intersect` b) && Iv.start b >= Iv.end a

filteredDomain2Intervalls :: [Int] -> [TestInterval]
filteredDomain2Intervalls [] = []
filteredDomain2Intervalls (x:xs) = map TestInterval (r xs x x)
    where r [] start last = [(start,last+1)]
          r (x:xs) start last | x == last + 1 = r xs start x
                              | otherwise = (start,last+1) : r xs x x

deleteRandom :: Eq a => [a] -> Gen [a]
deleteRandom list = do
  pos <- choose (0, length list - 1)
  return (delete (list !! pos) list)

monadRepeat :: (Monad m) => Int -> m a -> (a -> m a) -> m a
monadRepeat 0 x _ = x
monadRepeat n x fn = monadRepeat (n-1) (x >>= fn) fn

instance Arbitrary (Iv.AscendingIntervals TestInterval) where
    arbitrary     = do
      n <- choose(0,ivmax)
      list <- monadRepeat n (return domain) deleteRandom
      return (Iv.ascendingIntervals (filteredDomain2Intervalls list))

instance Arbitrary (Iv.AscendingPoints Int) where
    arbitrary     = do
      n <- choose(0,ivmax)
      list <- monadRepeat n (return domain) deleteRandom
      return (Iv.ascendingPoints list)

prop_groupPointsByIntervalls :: Iv.AscendingIntervals TestInterval -> Iv.AscendingPoints Int -> Bool
prop_groupPointsByIntervalls ivs xs = Iv.groupPointsByIntervalls ivs xs == safe_groupPointsByIntervalls ivs xs
    where safe_groupPointsByIntervalls ivs _ = map grap (Iv.getAscendingIntervals ivs)
          grap iv = Iv.ascendingPoints (filter (Iv.isPointInInterval iv) (Iv.getAscendingPoints xs))

mt :: (Iv.Interval a1 a, Ord a) => Iv.AscendingIntervals a1 -> Iv.AscendingPoints a -> [Iv.AscendingPoints a]
mt ivs xs = safe_groupPointsByIntervalls ivs xs
    where safe_groupPointsByIntervalls ivs _ = map grap (Iv.getAscendingIntervals ivs)
          grap iv = Iv.ascendingPoints (filter (Iv.isPointInInterval iv) (Iv.getAscendingPoints xs))

prop_ascendingIntervals2points :: Iv.AscendingIntervals TestInterval -> Bool
prop_ascendingIntervals2points ivs = Iv.ascendingIntervals2points ivs == safe_ascendingIntervals2points ivs
    where safe_ascendingIntervals2points ivs = let ivs' = Iv.getAscendingIntervals ivs
                                                in (Iv.ascendingPoints . sort . nub) (map Iv.start ivs' ++ map Iv.end ivs')
