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

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Interval (Point
                ,point
                ,Interval
                ,start
                ,end
                ,intersect
                ,isPointInInterval
                ,isStrictlyAfter
                ,ascendingIntervals
                ,getAscendingIntervals
                ,AscendingIntervals --only type not constructor
                ,ascendingPoints
                ,getAscendingPoints
                ,AscendingPoints --only type not constructor
                ,groupPointsByIntervalls
                ,ascendingIntervals2points
                ,divideInterval
                ,bestDiv
                ,locatePoint
                ,quantizeIv
                ) where
import Utils
import Data.List (sortBy)
import Data.Ord (comparing)

-- http://www.haskell.org/haskellwiki/Functional_dependencies
-- This tells Haskell that b is uniquely determined from a.
class (Num b) => Interval a b | a -> b where
    start :: a -> b
    end :: a -> b
    dur :: a -> b
    -- defaults
    dur x = end x - start x
    end x = start x + dur x

class Point a b | a -> b where
    point :: a -> b

instance (Num t) => Interval (t,t) t where
    start (x,_) = x
    end (_,x) = x

instance (Num t) => Point t t where
    point x = x

-- |Do the intervals a and b have common points?
intersect :: (Interval a a1, Ord a1) => a -> a -> Bool
intersect a b =
    let s1 = start a
        e1 = end a
        s2 = start b
        -- e2 = end b
    in
      if s1 > s2 then
          b `intersect` a
      else
          s2 < e1

-- |Is x in iv?
isPointInInterval :: (Interval a1 a, Ord a) => a1 -> a -> Bool
isPointInInterval iv x = start iv <= point x && point x < end iv

isStrictlyAfter :: (Interval a2 a, Interval a1 a, Ord a) => a2 -> a1 -> Bool
isStrictlyAfter a b = start b >= end a

data AscendingIntervals a = AscendingIntervals [a]
                            deriving Show

{-# ANN isAscendingIntervals "HLint: ignore Eta reduce" #-}
isAscendingIntervals :: (Interval b a, Ord a) => [b] -> Bool
isAscendingIntervals xs = isForAllNeighbours isStrictlyAfter xs

ascendingIntervals :: (Interval a1 a, Ord a) => [a1] -> AscendingIntervals a1
ascendingIntervals ivs =
    if not (isAscendingIntervals ivs) then
        error "not (isForAllNeighbours isStrictlyAfter ivs)"
    else
        AscendingIntervals ivs

getAscendingIntervals :: AscendingIntervals t -> [t]
getAscendingIntervals (AscendingIntervals xs) = xs

data AscendingPoints a = AscendingPoints [a]
                       deriving (Show, Eq)
ascendingPoints :: Ord a => [a] -> AscendingPoints a
ascendingPoints xs =
    if not (isForAllNeighbours (<) xs) then
        error "not (isForAllNeighbours (<) xs)"
    else
        AscendingPoints xs

getAscendingPoints :: AscendingPoints t -> [t]
getAscendingPoints (AscendingPoints xs) = xs

-- | For each interval return ascendingPoints that are all the points
--   from xs contained in the interval
groupPointsByIntervalls :: (Interval a1 a, Ord a) => AscendingIntervals a1 -> AscendingPoints a -> [AscendingPoints a]
groupPointsByIntervalls ivs xs = f (getAscendingIntervals ivs) (getAscendingPoints xs)
    where f [] _ = []
          f (_:ivs) [] = ascendingPoints [] : f ivs []
          f (iv:ivs) (x:xs)
              | point x < start iv = f (iv:ivs) xs
              | otherwise = ascendingPoints (takeWhile (< end iv) (x:xs)) :
                            f ivs (dropWhile (< end iv) (x:xs))

-- TODO call internal Constructor instead of safe ascendingPoints
ascendingIntervals2points :: (Interval a1 a, Ord a) => AscendingIntervals a1 -> AscendingPoints a
ascendingIntervals2points ivs = ascendingPoints (f (getAscendingIntervals ivs))
    where f (iv:ivs) = [start iv,end iv] ++ g ivs (end iv)
          f [] = []
          g [] _ = []
          g (iv:ivs) last = if start iv == last
                            then
                                end iv : g ivs (end iv)
                            else
                                [start iv,end iv] ++ g ivs (end iv)

divideInterval :: (Interval a b, Ord b, Fractional b, Enum b) => a -> b -> AscendingIntervals (b, b)
divideInterval iv n =
    let new_dur = dur iv / n
        points = map ((+ start iv) . (*new_dur)) [0..n]
    in ascendingIntervals (neighbours points)

-- min cost to move x to start or end of iv
boundaryMoveCost :: (Interval a a1, Ord a1) => a -> a1 -> a1
boundaryMoveCost iv x = let x' = point x
                            dist = min (abs (start iv - x')) (abs (end iv) - x')
                        in dist * dist

-- what is the cost of dividing iv by div (e.g. 3) and moving points
-- in xs accordingly
divCost :: (Interval a b, Ord b, Fractional b, Enum b) => a -> AscendingPoints b -> b -> b
divCost iv xs div = let small_ivs = divideInterval iv div
                        point_groups = groupPointsByIntervalls small_ivs xs
                        group_cost (small_iv,points) =
                          sum (map (boundaryMoveCost small_iv) (getAscendingPoints points))
                    in sum (zipWith (curry group_cost) (getAscendingIntervals small_ivs) point_groups)

-- return a list of pairs (div,cost). Best pair comes first. In case of
-- two identical costs the div that comes first in divs will be
-- prefered (sortBy is a stable sorting algorithm).
rankedDivs :: (Interval a1 a, Ord a, Fractional a, Enum a) => a1 -> AscendingPoints a -> [a] -> [(a, a)]
rankedDivs iv xs divs = sortBy test (zip divs (map (divCost iv xs) divs))
  where test (_,a) (_,b) = compare a b

-- choose the best div from divs
bestDiv :: Interval a1 Float => [Int] -> a1 -> AscendingPoints Float -> Int
bestDiv divs iv xs = round (fst (head (rankedDivs iv xs (map intToFloat divs)))) :: Int

-- TODO implement this as a binary search
locatePoint :: (Interval t a1, Show t, Ord a1, Num a, Eq t, Show a, Show a1) => AscendingIntervals t -> a1 -> (t, (a, a))
locatePoint ivs x = r (getAscendingIntervals ivs) (point x) 0
    where r (iv:ivs) x index
              | isPointInInterval iv x ||
                ((ivs == []) && (x >= end iv)) = (iv,(index,index+1))
              | otherwise = r ivs x (index+1)
          r a b c = error $ "locatePoint " ++ show a ++ " " ++ show b ++ " " ++ show c

quantizeIv :: (Interval a1 t1, Interval a a2, Ord a2, Show a2) =>
              ((t1, t1) -> a -> t) -> AscendingIntervals a1 -> AscendingIntervals (a2, a2) -> a -> t
quantizeIv f rational_ivs ivs iv =
    let rivs = getAscendingIntervals rational_ivs
        s = start iv
        e = end iv
        ((s0,s1),(si0,si1)) = locatePoint ivs s
        ((e0,e1),(ei0,ei1)) = locatePoint ivs e
        result = [(cost ds de,(end3 a,end3 b)) |
                     a <- [(s0,si0,start $ rivs !! si0),
                           (s1,si1,end $ rivs !! si0)],
                     b <- [(e0,ei0,start $ rivs !! ei0),
                           (e1,ei1,end $ rivs !! ei0)],
                     mid3 a /= mid3 b,
                     let ds = abs (s - fst3 a),
                     let de = abs (e - fst3 b)]
    in f (snd $ best result) iv
    where cost ds de = abs (de - ds) + abs ds + abs de
          test = comparing fst
          best xs = head (sortBy test xs)
          fst3 (x,_,_) = x
          mid3 (_,x,_) = x
          end3 (_,_,x) = x
