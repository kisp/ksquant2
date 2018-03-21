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

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Quantize (bestDiv, quantizeIv)

where

import qualified Types as T (DivChoices, Div, WRat, Time, TimeInterval)
import qualified Interval as Iv (Interval
                                , AscendingPoints
                                , AscendingIntervals
                                , getAscendingIntervals
                                , getAscendingPoints
                                , start
                                , end
                                , locatePoint
                                , point
                                , divideInterval
                                , groupPointsByIntervalls)

import Data.List (sortBy,minimumBy)
import Data.Ord (comparing)

type Cost = T.WRat

-- | Choose the best div from divs.
bestDiv :: T.DivChoices -> T.TimeInterval -> Iv.AscendingPoints T.Time -> T.Div
bestDiv divChoices iv xs = fst (head (rankedDivs iv xs (map fromInteger divChoices)))

-- return a list of pairs (div,cost). Best pair comes first. In case of
-- two identical costs the div that comes first in divs will be
-- prefered (sortBy is a stable sorting algorithm).
rankedDivs :: T.TimeInterval -> Iv.AscendingPoints T.Time -> T.DivChoices -> [(T.Div, Cost)]
rankedDivs iv xs divChoices = sortBy test (zip divChoices costs)
  where test (_,a) (_,b) = compare a b
        costs :: [Cost]
        costs = map (divCost iv xs) divChoices

quantizeIv :: (Iv.Interval (a, a) a, Iv.Interval t2 a, Iv.Interval t t1, Show a, Ord a) =>
  ((t1, t1) -> t2 -> t3) -> Iv.AscendingIntervals t -> Iv.AscendingIntervals (a, a) -> t2 -> t3
quantizeIv f rational_ivs ivs iv =
    let rivs = Iv.getAscendingIntervals rational_ivs
        s = Iv.start iv
        e = Iv.end iv
        ((s0,s1),(si0,si1)) = Iv.locatePoint ivs s
        ((e0,e1),(ei0,ei1)) = Iv.locatePoint ivs e
        result = [(cost ds de,(end3 a,end3 b)) |
                     a <- [(s0,si0,Iv.start $ rivs !! si0),
                           (s1,si1,Iv.end $ rivs !! si0)],
                     b <- [(e0,ei0,Iv.start $ rivs !! ei0),
                           (e1,ei1,Iv.end $ rivs !! ei0)],
                     mid3 a /= mid3 b,
                     let ds = abs (s - fst3 a),
                     let de = abs (e - fst3 b)]
    in f (snd $ best result) iv
    where cost ds de = abs (de - ds) + abs ds + abs de
          test = comparing fst
          best = minimumBy test
          fst3 (x,_,_) = x
          mid3 (_,x,_) = x
          end3 (_,_,x) = x

-- min cost to move x to start or end of iv
boundaryMoveCost :: (Iv.Interval a a1, Ord a1) => a -> a1 -> a1
boundaryMoveCost iv x = let x' = Iv.point x
                            dist = min (abs (Iv.start iv - x'))
                                       (abs (Iv.end iv) - x')
                        in dist * dist

-- what is the cost of dividing iv by div (e.g. 3) and moving points
-- in xs accordingly
divCost :: T.TimeInterval -> Iv.AscendingPoints T.Time -> T.Div -> Cost
divCost iv xs div = let small_ivs = Iv.divideInterval iv div
                        small_ivs :: Iv.AscendingIntervals (T.WRat, T.WRat)
                        point_groups = Iv.groupPointsByIntervalls small_ivs xs
                        group_cost (small_iv,points) =
                          sum (map (boundaryMoveCost small_iv)
                                (Iv.getAscendingPoints points))
                    in sum (zipWith (curry group_cost)
                                    (Iv.getAscendingIntervals small_ivs)
                                    point_groups)
