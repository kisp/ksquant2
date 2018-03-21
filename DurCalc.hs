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

module DurCalc (notableDur
               , notableDurL
               , divToRatio
               , dotFactor
               , exp2
               , isExp2)
where

import Data.Ratio ( numerator, denominator, (%) )

-- | Compute @2^n@ for any integer @n@.
exp2 :: Integer -> Rational
exp2 n | n >= 0    = 2^n
       | otherwise = 1 / (2^abs n)

isExp2 :: Rational -> Bool
isExp2 r | r == 1 = True
         | r > 1 && denominator r == 1 = isPowerOfTwo (numerator r)
         | r < 1 && numerator r == 1   = isPowerOfTwo (denominator r)
         | otherwise = False

isPowerOfTwo :: Integer -> Bool
isPowerOfTwo 1 = True
isPowerOfTwo x | x > 1 = even x && isPowerOfTwo (x `div` 2)
isPowerOfTwo _ = error  "isPowerOfTwo"

lowerPowerOfTwo :: Integer -> Integer
lowerPowerOfTwo 1 = 1
lowerPowerOfTwo x | x > 1 = if isPowerOfTwo x then
                                x
                            else
                                lowerPowerOfTwo (x-1)
lowerPowerOfTwo _ = error "lowerPowerOfTwo"

-- e.g. when we divide by 9 the tuplet ratio will be 8 % 9
divToRatio :: Integer -> Rational
divToRatio d = lowerPowerOfTwo d % d

-- notableDur' :: Rational -> Bool
-- notableDur' x = h (numerator x) (denominator x)
--     where h 1 d = isPowerOfTwo d
--           h 3 d = isPowerOfTwo d && d >= 2
--           h 7 d = isPowerOfTwo d && d >= 4
--           -- 15 = 1 + 2 + 4 + 8
--           h 15 d = isPowerOfTwo d && d >= 8
--           h _ _ = False

notableDur' :: Integer -> Rational -> Bool
notableDur' maxDots r | maxDots == 0 = isExp2 r
                      | maxDots > 0  = notableDur' (maxDots - 1) r ||
                                       notableDur' 0 (r / dotFactor maxDots)
                      | otherwise    = error "notableDur' maxDots < 0"

notableDur :: Integer -> Rational -> Bool
notableDur maxDots x = notableDur' maxDots (abs x)

notableDurL :: Rational -> Rational -> Bool
notableDurL l x | abs x <= abs l = notableDur 3 x
                | otherwise = False

-- | Compute a factor for a given number of augmentation dots.
--
-- The duration of a dotted note with duration @a@ and @n@ dots can be
-- obtained by @a * 'dotFactor' n@.
--
--
-- Examples:
--
-- >>> dotFactor 0
-- 1
--
-- >>> dotFactor 1
-- 3 % 2
--
-- >>> dotFactor 2
-- 7 % 4
--
dotFactor :: Integer  -- ^ number of augmentation dots
          -> Rational -- ^ factor
dotFactor n | n >= 0 = 2 - (1 % (2^n))
            | otherwise = error "dotFactor not defined on negative n"
