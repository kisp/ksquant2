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

{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Measure (m
               , l
               , d
               , r
               , dur
               , E(L,D,R)
               , M(M)
               , Ms
               , transform_leafs
               , transform_leafs'
               , measuresDivideLeafs
               , measuresWithBeats
               , leafDurs
               , leafEffectiveDurs
               , measuresLeafIntervals
               , measureNumLeaf
               , measuresTransformLeafs
               , wrapWithD
               , Score
               , Part
               , Voice
               , Label
               , measuresTieOrRest
               , measuresUntilTime
               , measureDur
               , labelVoice
               , mleaves
               , vleaves
               , eid)

where

import Types (Timesig, Tempo, InfInt, WRat, WInt)
import Data.Ratio ( (%) )
import Utils (dxsToXs, neighbours)
import qualified AbstractScore as A (Score, Part, Voice)
import qualified Interval as Iv (Interval, end, isPointInInterval)
import Data.List (find)
import qualified Lisp as L (LispVal, readLisp')
import DurCalc (notableDurL, divToRatio)

type Score = A.Score Ms
type Part = A.Part Ms
type Voice = A.Voice Ms

type Ms = [M]

data M = M Timesig Tempo E
       deriving (Show, Eq)

type Label = InfInt
type Notes = L.LispVal
type Expressions = L.LispVal

data E =
--    dur      factor   children
    D Rational Rational [E]
--         dur      tie
       | L Rational Bool Label Notes Expressions
--         dur
       | R Rational Label
         deriving (Show,Eq)

dur :: E -> Rational
dur (D d _ _) = d
dur (L d _ _ _ _)   = d
dur (R d _)     = d

eid :: E -> Label
eid D{} = error "id not for D"
eid (R _ x) = x
eid (L _ _ x _ _) = x

timesigDur :: (WInt, WInt) -> WRat
timesigDur (n,d) = n%d

m :: (Integer, Integer) -> Tempo -> E -> M
m timesig tempo d = if not(check timesig tempo d) then
                        error "m timesig tempo d not valid"
                    else M timesig tempo d
    where check timesig _ d = dur d == timesigDur timesig

l :: Rational -> Bool -> Notes -> Expressions -> E
l d tie notes expressions =
    if not check then
        error $ "cannot construct L from " ++ show d ++ " " ++ show tie
    else L d tie 0 notes expressions
    where check = notableDurL 1 d

r :: Rational -> E
r d = if not check then
          error $ "r d not valid: " ++ show d
      else R d 0
    where check = notableDurL 1 d

d :: Rational -> Rational -> [E] -> E
d d r es = if not(check d r es) then
          error "d d r es not valid"
      else D d r es
    where check d r es = d == r * sum (map dur es)

class Transformable a b where
    transform_leafs :: (b -> b) -> a -> a
    -- passing around user supplied data
    transform_leafs' :: (b -> t -> (b,t)) -> t -> a -> (a,t)

smap :: (t -> a -> (b,t)) -> [a] -> t -> ([b],t)
smap _ [] d   = ([],d)
smap f (x:xs) d = let (r,newd) = f d x
                  in let (rest,lastd) = smap f xs newd
                     in (r : rest,lastd)

instance Transformable M E where
    transform_leafs fn (M timesig tempo e) =
        m timesig tempo (transform_leafs fn e)
    transform_leafs' fn z (M timesig tempo e) =
        let (r,z') = transform_leafs' fn z e
        in (m timesig tempo r,z')

instance Transformable E E where
    transform_leafs fn (D dur r es) =
        d dur r (map (transform_leafs fn) es)
    transform_leafs fn x = fn x
    transform_leafs' fn z (D dur r es) =
        let (res,z') = smap (transform_leafs' fn) es z
        in (d dur r res,z')
    transform_leafs' fn z x = fn x z

n60 :: L.LispVal
n60 = L.readLisp' "(60)"
nil :: L.LispVal
nil = L.readLisp' "()"

measuresDivideLeafs :: Transformable b E => [b] -> [Integer] -> [b]
measuresDivideLeafs ms divs =
    fst (smap (transform_leafs' trans) ms divs)
        where
              trans (L dur _ _ _ _) (n:ds) =
                  let r = if notableDurL 1 (dur / (n%1)) then 1 else divToRatio n
                  in (d dur r (replicate (fromInteger n) (l (dur / (n % 1) / r) False n60 nil)),
                        ds)
              trans r@(R _ _) ds = (r,ds)
              trans D{} _ = error "measuresDivideLeafs: did not expect (D _ _ _)"
              trans L{} [] = error "measuresDivideLeafs: divs have run out"

measuresWithBeats :: [(Integer, Integer)] -> [Tempo] -> Ms
measuresWithBeats timesigs tempos =
    let divs = map fst timesigs
    in measuresDivideLeafs (zipWith (curry mes) timesigs tempos) divs
    where mes (timesig,tempo) =
              -- use L here, so that we are allowed to have a duration of e.g. 5/4
              m timesig tempo (L (timesigDur timesig) False 0 n60 nil)

eleaves :: E -> [E]
eleaves e@L{} = [e]
eleaves e@(R _ _) = [e]
eleaves (D _ _ es) = concatMap eleaves es

mleaves :: M -> [E]
mleaves (M _ _ e) = eleaves e

vleaves :: Ms -> [E]
vleaves = concatMap mleaves

leafDurs :: E -> [Rational]
leafDurs e = map dur (eleaves e)

leafEffectiveDurs :: E -> [Rational]
leafEffectiveDurs = leafEffectiveDurs' 1
    where
      leafEffectiveDurs' r (L d _ _ _ _) = [d * r]
      leafEffectiveDurs' r (R d _) = [d * r]
      leafEffectiveDurs' r (D _ r' es) =
          concatMap (leafEffectiveDurs' (r * r')) es

measureDur :: M -> Rational
measureDur (M timesig@(n,_) tempo _) =  (n%1) * tempoToBeatDur timesig tempo
    where
      tempoToBeatDur (_,d) (td,bps) = (4%d) * (60 / bps) * (4%td)

-- foldl            :: (a -> b -> a) -> a -> [b] -> a
-- foldl f acc []     =  acc
-- foldl f acc (x:xs) =  foldl f (f acc x) xs

measuresStartTimes :: Ms -> [Rational]
measuresStartTimes ms = dxsToXs (map measureDur ms)

measuresUntilTime :: (RealFrac t) => Ms -> t -> Ms
measuresUntilTime ms time = map fst (takeWhile p (zip ms (measuresStartTimes ms)))
    where p (_,s) = fromRational s < time

-- measure_leaf_start_times (M (_,d) tempo div) start =
--     (map (+start) (dxsToXs_butlast (map trans (leafEffectiveDurs div))))
--     where trans dur = (tempoToBeatDur tempo) * (dur_to_beat dur)
--           dur_to_beat dur = dur * (d%1)
--           butlast xs = reverse (tail (reverse xs))
--           dxsToXs_butlast dxs = butlast (dxsToXs dxs)


-- measures_leaf_start_times ms = (concatMap (uncurry measure_leaf_start_times)
--                                               (zip ms (measuresStartTimes ms)))

measureLeafIntervals :: M -> Rational -> [(Rational, Rational)]
measureLeafIntervals (M (_,d) tempo div) start =
    neighbours (map (+start) (dxsToXs (map trans (leafEffectiveDurs div))))
    where trans dur = tempoToBeatDur tempo * dur_to_beat dur * (4%d)
          dur_to_beat dur = dur * (d%1)
          tempoToBeatDur (d,tempo) = 60 / tempo / (d%4)

measuresLeafIntervals :: Ms -> [(Rational, Rational)]
measuresLeafIntervals ms = concatMap (uncurry measureLeafIntervals)
                              (zip ms (measuresStartTimes ms))

measureNumLeaf :: M -> Int
measureNumLeaf m = length (measureLeafIntervals m 0)

-- if leaf start time is not in any iv, then rest. if leaf start time
-- is in an interval keep it and make it a tie if the end time is not
-- the same as the end time of the interval
measuresTieOrRest :: (Transformable a1 E, Iv.Interval a b, Ord b) =>
                        [a1] -> [a] -> [(b, b)] -> [a1]
measuresTieOrRest ms ivs leaf_times =
    fst (smap (transform_leafs' trans) ms leaf_times)
        where
              trans (L dur _ _ notes expressions) ((s,e):leaf_times) =
                  case find ivContainsStart ivs of
                    Nothing -> (r dur,leaf_times)
                    Just iv -> let tie = e < Iv.end iv
                               in (l dur tie notes expressions,leaf_times)
                  where ivContainsStart = flip Iv.isPointInInterval s
              trans L{} [] = error "measuresTieOrRest: leaf_times have run out"
              trans (R _ _) _    = error "measuresTieOrRest: did not expect a rest here"
              trans D{} _ = error "measuresTieOrRest: did not expect a D"

measuresTransformLeafs :: (Transformable a1 E, Iv.Interval a b, Ord b) =>
                        (E -> a -> E) -> [a1] -> [a] -> [(b, b)] -> [a1]
measuresTransformLeafs f ms ivs leaf_times =
    fst (smap (transform_leafs' trans) ms leaf_times)
        where
              trans D{} _ = error "measuresTieOrRest: did not expect a D"
              trans _ [] = error "measuresTransformLeafs: leaf_times have run out"
              trans elt ((s,_):leaf_times) =
                  case find ivContainsStart ivs of
                    Nothing -> (elt,leaf_times)
                    Just iv -> (f elt iv,leaf_times)
                  where ivContainsStart = flip Iv.isPointInInterval s

labelVoice :: Transformable a E => [a] -> [a]
labelVoice voice =
    fst (smap (transform_leafs' trans) voice [0..])
        where
              trans (L dur tie _ notes expressions) (id:ids) = (L dur tie id notes expressions,ids)
              trans (R dur _) (id:ids) = (R dur id,ids)
              trans D{} _ = error "labelVoice: did not expect a D"
              trans _ [] = error "labelVoice: ids have run out? (should never happen)"

wrapWithD :: E -> E
wrapWithD e = d (dur e) 1 [e]

---------------------------------------------------------

-- m1 = M (4,4) (60 % 1)
--      (D (1 % 1) (1 % 1)
--       [L (1 % 4) False,L (1 % 4) False,L (1 % 4) False,L (1 % 4) False])

-- m2 = (measuresDivideLeafs [m1] (repeat 3)) !! 0
-- m3 = (measuresDivideLeafs [m2] (repeat 3)) !! 0
