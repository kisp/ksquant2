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

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module MeasureToLily (mToLily, vToLily)

where

import qualified Measure as M (Ms, M(M), E(L,R,D))
import qualified Lily as L (Dur(Dur)
                           , Pitch(Pitch)
                           , Name(B, C, D, E, F) -- A G
                           , Accidental(Natural, Sharp) -- Flat
                           , powerToSimpleDur
                           , Elt(Chord, Rest, Times)
                           , Measure(Measure)
                           , Measures)
import Data.Ratio
import Lisp

log2 :: (Num a, Integral a1) => a1 -> a
log2 1 = 0
log2 n | even n = 1 + log2 (div n 2)
log2 _ = error "log2"

midiToLily :: (Num a, Eq a, Show a) => a -> L.Pitch
midiToLily 59 = L.Pitch L.B L.Natural 3
midiToLily 60 = L.Pitch L.C L.Natural 4
midiToLily 61 = L.Pitch L.C L.Sharp 4
midiToLily 62 = L.Pitch L.D L.Natural 4
midiToLily 64 = L.Pitch L.E L.Natural 4
midiToLily 65 = L.Pitch L.F L.Natural 4
midiToLily x = error $ "midiToLily not implemented: " ++ show x

durToLily :: Rational -> L.Dur
durToLily d = L.Dur (base d) (dots d)
    where dots d = case numerator d of
                     1 -> 0
                     3 -> 1
                     7 -> 2
                     15 -> 3
                     _ -> error "durToLily"
          base d = L.powerToSimpleDur (log2 (denominator d) - dots d)

ensureListOfIntegers :: LispVal -> [Int]
ensureListOfIntegers (LispList xs) =
    map ensureInt xs
    where ensureInt (LispInteger x) = fromInteger x
          ensureInt _ = error "ensureInt"
ensureListOfIntegers _ = error "ensureListOfIntegers"

eToLily :: M.E -> [L.Elt]
eToLily (M.L d tie _ notes _) = [L.Chord (durToLily d) midis tie]
  where midis = map midiToLily (ensureListOfIntegers notes)
eToLily (M.R d _) = [L.Rest (durToLily d)]
eToLily (M.D _ r es) | r == 1 = concatMap eToLily es
                       | otherwise = let n' = fromInteger (numerator r)
                                         d' = fromInteger (denominator r)
                                     in [L.Times n' d' (concatMap eToLily es)]

mToLily :: M.M -> L.Measure
mToLily (M.M (n,d) _ e) = L.Measure (fromInteger n) (fromInteger d) (eToLily e)

vToLily :: M.Ms -> L.Measures
vToLily = map mToLily
