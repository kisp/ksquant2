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
import qualified Lily as Ly (Dur(Dur)
                            , Pitch(Pitch)
                            , Name(B, C, D, E, F) -- A G
                            , Accidental(Natural, Sharp) -- Flat
                            , powerToSimpleDur
                            , Elt(Chord, Rest, Times)
                            , Measure(Measure)
                            , Measures)
import Data.Ratio (numerator, denominator)
import qualified Lisp as L ( LispVal(LispList,LispInteger) )

log2 :: (Num a, Integral a1) => a1 -> a
log2 1 = 0
log2 n | even n = 1 + log2 (div n 2)
log2 _ = error "log2"

midiToLily :: (Num a, Eq a, Show a) => a -> Ly.Pitch
midiToLily 59 = Ly.Pitch Ly.B Ly.Natural 3
midiToLily 60 = Ly.Pitch Ly.C Ly.Natural 4
midiToLily 61 = Ly.Pitch Ly.C Ly.Sharp 4
midiToLily 62 = Ly.Pitch Ly.D Ly.Natural 4
midiToLily 64 = Ly.Pitch Ly.E Ly.Natural 4
midiToLily 65 = Ly.Pitch Ly.F Ly.Natural 4
midiToLily x = error $ "midiToLily not implemented: " ++ show x

durToLily :: Rational -> Ly.Dur
durToLily d = Ly.Dur (base d) (dots d)
    where dots d = case numerator d of
                     1 -> 0
                     3 -> 1
                     7 -> 2
                     15 -> 3
                     _ -> error "durToLily"
          base d = Ly.powerToSimpleDur (log2 (denominator d) - dots d)

ensureListOfIntegers :: L.LispVal -> [Int]
ensureListOfIntegers (L.LispList xs) =
    map ensureInt xs
    where ensureInt (L.LispInteger x) = fromInteger x
          ensureInt _ = error "ensureInt"
ensureListOfIntegers _ = error "ensureListOfIntegers"

eToLily :: M.E -> [Ly.Elt]
eToLily (M.L d tie _ notes _) = [Ly.Chord (durToLily d) midis tie]
  where midis = map midiToLily (ensureListOfIntegers notes)
eToLily (M.R d _) = [Ly.Rest (durToLily d)]
eToLily (M.D _ r es) | r == 1 = concatMap eToLily es
                     | otherwise = let n' = fromInteger (numerator r)
                                       d' = fromInteger (denominator r)
                                   in [Ly.Times n' d' (concatMap eToLily es)]

mToLily :: M.M -> Ly.Measure
mToLily (M.M (n,d) _ e) = Ly.Measure (fromInteger n) (fromInteger d) (eToLily e)

vToLily :: M.Ms -> Ly.Measures
vToLily = map mToLily
