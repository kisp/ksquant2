-- This file is part of KSQuant2.

-- Copyright (c) 2010, Kilian Sprotte. All rights reserved.

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
module Utils where
import Data.Ratio
import Debug.Trace

neighbours list = zip list (tail list)

isForAllNeighbours p list = all (uncurry p) (neighbours list)

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

-- TODO remove me
-- expand :: Rational -> Rational -> (Integer,Integer)
expand a b = let f = lcm (denominator a) (denominator b)
             in (a*(f%1),b*(f%1))

expand' xs = let f = foldl1 lcm (map denominator xs) % 1
             in sum (map (numerator . (*f)) xs)

mtr x = trace (show x) x
