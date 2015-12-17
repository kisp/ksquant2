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
module Utils (Err,neighbours,isForAllNeighbours,intToFloat,stickToLast)
where

-- | Computation result of type 'a' or error as a String.
type Err a = Either String a

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

-- | Coerce given integer to float.
--
-- >>> intToFloat 3
-- 3.0
intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

-- | Build an infinite list by endlessly repeating the last element.
stickToLast :: [a] -> [a]
stickToLast list = list ++ repeat (last list)
