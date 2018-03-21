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

-- | Some utilities.
module Utils (neighbours
             , isForAllNeighbours
             , stickToLast
             , repeatList
             , rationalPairToTimePair
             , appendNewline
             , dxsToXs
             , oneOfEq)
where

import Types (Time)

-- | Return adjacent elements as pairs.
--
-- >>> neighbours [1,2,3]
-- [(1,2),(2,3)]
-- >>> neighbours [1,2]
-- [(1,2)]
-- >>> neighbours [1]
-- []
-- >>> neighbours []
-- []
neighbours :: [a] -> [(a, a)]
neighbours list = zip list (tail list)

-- | Test if the given binary predicate is satisfied for
-- all neighbours of the given list.
--
-- >>> isForAllNeighbours (<) [1,2,3]
-- True
isForAllNeighbours :: (a -> a -> Bool) -> [a] -> Bool
isForAllNeighbours p list = all (uncurry p) (neighbours list)

-- | Build an infinite list by endlessly repeating the last element.
stickToLast :: [a] -> [a]
stickToLast list = list ++ repeat (last list)

-- | Expand first list by repeating elements according
-- to the repetition counts given by the second list.
repeatList :: (Num b, Ord b) => [a] -> [b] -> [a]
repeatList [] _ = []
repeatList _ [] = []
repeatList (x:xs) (1:rs) = x:repeatList xs rs
repeatList (x:xs) (r:rs) | r > 1 = x:repeatList (x:xs) (r-1:rs)
                     | r < 1 = repeatList xs rs
                     | otherwise = undefined

rationalToTime :: Rational -> Time
rationalToTime = fromRational

rationalPairToTimePair :: (Rational, Rational) -> (Time, Time)
rationalPairToTimePair (x,y) = (rationalToTime x, rationalToTime y)

appendNewline :: String -> String
appendNewline s = s ++ "\n"

dxsToXs :: [Rational] -> [Rational]
dxsToXs = scanl (+) 0

oneOfEq :: (Show a, Eq a) => a -> a -> a
oneOfEq a b | a == b = a
            | otherwise = error $ "expected to be Eq: " ++ show a ++ " " ++ show b
