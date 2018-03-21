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

module MeasureToEnp (vToEnp, mToEnp)

where

import qualified Measure as M ( M(M), E(D,R,L), Label, dur, Ms, labelVoice, vleaves, eid )
import qualified Enp as E ( Elt(..), EnpDur, Measure, dur, scaleElt, makeMeasure, Measures )
import qualified Lisp as L ( LispVal, readLisp' )
import Data.Ratio ( Ratio, denominator, (%), numerator )

durs2factor :: Integral a => [Ratio a] -> Ratio a
durs2factor xs = foldl1 lcm (map denominator xs) % 1
ratio2integer :: Integral a => Ratio a -> a
ratio2integer r | denominator r == 1 = numerator r
                | otherwise = error "ratio2integer: not an integer"

tiedOverFromLast :: (Num a, Eq a) => [(a, M.E)] -> a -> Bool
tiedOverFromLast _ 0 = False
tiedOverFromLast assoc id =
    case lookup (id-1) assoc of
      Nothing -> error "tiedOverFromLast not found"
      Just x -> has_forward_tie x
    where has_forward_tie (M.R _ _) = False
          has_forward_tie (M.L _ tie _ _ _) = tie
          has_forward_tie M.D{} = error "tiedOverFromLast (M.D _ _ _)"

expressionsForChord :: Bool -> L.LispVal -> L.LispVal
expressionsForChord True _            = L.readLisp' "()"
expressionsForChord False expressions = expressions

eToEnp :: [(M.Label, M.E)] -> Rational -> M.E -> E.Elt
eToEnp assoc f (M.L d _ id notes expressions) =
  E.Chord (ratio2integer (d * f)) tofl notes (expressionsForChord tofl expressions)
  where tofl = tiedOverFromLast assoc id
eToEnp _ f (M.R d _) = E.Rest (ratio2integer (d * f))
eToEnp assoc f (M.D d _ es) =
    let f' = durs2factor (map M.dur es)
    in E.Div (ratio2integer (d * f))
           (map (eToEnp assoc f') es)

unwrap :: E.Elt -> [E.Elt]
unwrap (E.Div _ e) = e
unwrap _ = error "unwrap: not a Div"

wrapIfNeeded :: E.Elt -> E.Elt
wrapIfNeeded e@(E.Div _ _) = e
wrapIfNeeded x = E.Div 1 [x]

adaptForTimesig :: (E.EnpDur, t) -> [E.Elt] -> [E.Elt]
adaptForTimesig (n,_) es = let f = n `div` sum (map E.dur es)
                           in map (E.scaleElt f) es

mToEnp :: [(M.Label, M.E)] -> M.M -> E.Measure
mToEnp assoc (M.M ts t e) =
    let f = denominator (M.dur e) % 1
        list = unwrap (wrapIfNeeded (eToEnp assoc f e))
        list' = map wrapIfNeeded list
        list'' = adaptForTimesig ts list'
    in E.makeMeasure ts t list''

vToEnp :: M.Ms -> E.Measures
vToEnp v = let v' = M.labelVoice v
               ls = M.vleaves v'
               assoc = zip (map M.eid ls) ls
           in map (mToEnp assoc) v'
