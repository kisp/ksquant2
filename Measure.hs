module Measure (m
               ,l
               ,d
               ,r
               ,dur
               ,E(L,D,R)
               ,M(M)
               ,transform_leafs
               ,transform_leafs'
               ,measures_divide_leafs
               ,measures_with_beats
               ,leaf_durs
               ,leaf_effective_durs
               ,measures_leaf_intervals
               ,wrapWithD
               )
where
import Data.Ratio
import Utils

isPowerOfTwo 1 = True
isPowerOfTwo x | x > 1 = if (even x) then
                             isPowerOfTwo (x `div` 2)
                         else
                             False

lowerPowerOfTwo 1 = 1
lowerPowerOfTwo x | x > 1 = if isPowerOfTwo x then
                                x
                            else
                                (lowerPowerOfTwo (x-1))


-- e.g. when we divide by 9 the tuplet ratio will be 8 % 9
div_to_ratio :: Integer -> Rational
div_to_ratio d = ((lowerPowerOfTwo d) % d)

notableDur :: Rational -> Bool
notableDur x = h (numerator x) (denominator x)
    where h 1 d = isPowerOfTwo d
          h 3 d = isPowerOfTwo d && d >= 2
          h 7 d = isPowerOfTwo d && d >= 4
          -- 15 = 1 + 2 + 4 + 8
          h 15 d = isPowerOfTwo d && d >= 8
          h _ _ = False

type Timesig = (Integer,Integer)
type Tempo = Rational

data M = M Timesig Tempo E
       deriving (Show,Eq)

mdiv (M _ _ e) = e

data E =
--    dur      factor   children
    D Rational Rational [E]
--         dur      tie
       | L Rational Bool
--         dur
       | R Rational
         deriving (Show,Eq)

dur (D d _ _) = d
dur (L d _)   = d
dur (R d)     = d

timesig_dur (n,d) = (n%d)

m timesig tempo d = if not(check timesig tempo d) then
                        error "m timesig tempo d not valid"
                    else M timesig tempo d
    where check timesig tempo d = (dur d) == (timesig_dur timesig)

l d tie = if not(check) then
          error "l d tie not valid"
      else L d tie
    where check = notableDur d

r d = if not(check) then
          error "r d not valid"
      else R d
    where check = notableDur d

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
                  in let (rest,lastd) = (smap f xs newd)                      
                     in ((r : rest),lastd)

instance Transformable M E where
    transform_leafs fn (M timesig tempo e) =
        m timesig tempo (transform_leafs fn e)
    transform_leafs' fn z (M timesig tempo e) =
        let (r,z') = (transform_leafs' fn z e)
        in ((m timesig tempo r),z')

instance Transformable E E where
    transform_leafs fn (D dur r es) =
        d dur r (map (transform_leafs fn) es)
    transform_leafs fn x = fn x
    transform_leafs' fn z (D dur r es) =
        let (res,z') = (smap (transform_leafs' fn) es z)
        in ((d dur r res),z')
    transform_leafs' fn z x = fn x z

measures_divide_leafs ms divs =
    (fst (smap (transform_leafs' trans) ms divs))
        where 
              trans (L dur tie) (n:ds) =
                  let r = if (notableDur (dur / (n%1))) then 1 else div_to_ratio n
                  in (d dur r (take (fromInteger n) (repeat (l ((dur/(n%1)/r)) False))),
                        ds)
              trans r@(R dur) ds = (r,ds)

measures_with_beats timesigs tempos =
    let divs = map fst timesigs
    in measures_divide_leafs (map mes (zip timesigs tempos)) divs
    where mes (timesig,tempo) =
              (m timesig tempo (l (timesig_dur timesig) False))

leaf_durs :: E -> [Rational]
leaf_durs (L d _) = [d]
leaf_durs (R d) = [d]
leaf_durs (D _ _ es) = concatMap leaf_durs es

leaf_effective_durs :: E -> [Rational]
leaf_effective_durs x = leaf_effective_durs' 1 x
    where
      leaf_effective_durs' r (L d _) = [d * r]
      leaf_effective_durs' r (R d) = [d * r]
      leaf_effective_durs' r (D _ r' es) =
          concatMap (leaf_effective_durs' (r * r')) es

tempo_to_beat_dur :: Rational -> Rational
tempo_to_beat_dur tempo = 60 / tempo

measure_dur (M (n,_) tempo _) = (n%1) * (tempo_to_beat_dur tempo)

-- foldl            :: (a -> b -> a) -> a -> [b] -> a
-- foldl f acc []     =  acc
-- foldl f acc (x:xs) =  foldl f (f acc x) xs

dxs_to_xs dxs = scanl (+) 0 dxs
butlast xs = reverse (tail (reverse xs))
dxs_to_xs_butlast dxs = butlast (dxs_to_xs dxs)

measures_start_times ms = dxs_to_xs (map measure_dur ms)

measure_leaf_start_times m@(M (n,d) tempo div) start =
    (map (+start) (dxs_to_xs_butlast (map trans (leaf_effective_durs div))))
    where trans dur = (tempo_to_beat_dur tempo) * (dur_to_beat dur)
          dur_to_beat dur = dur * (d%1)

measures_leaf_start_times ms = (concatMap (uncurry measure_leaf_start_times)
                                              (zip ms (measures_start_times ms)))

measure_leaf_intervals m@(M (n,d) tempo div) start =
    (neighbours (map (+start) (dxs_to_xs (map trans (leaf_effective_durs div)))))
    where trans dur = (tempo_to_beat_dur tempo) * (dur_to_beat dur)
          dur_to_beat dur = dur * (d%1)

measures_leaf_intervals ms = (concatMap (uncurry measure_leaf_intervals)
                              (zip ms (measures_start_times ms)))

-- if leaf start time is not in any iv, then rest. if leaf start time
-- is in an interval keep it and make it a tie if the end time is not
-- the same as the end time of the interval
-- TODO unfinished
measures_tie_or_rest ms ivs =
    map (transform_leafs trans) ms
        where 
              trans (L dur tie) = (L dur tie)
              trans r@(R dur) = r

wrapWithD :: E -> E
wrapWithD e = d (dur e) 1 [e]

---------------------------------------------------------

m1 = M (4,4) (60 % 1)
     (D (1 % 1) (1 % 1)
      [L (1 % 4) False,L (1 % 4) False,L (1 % 4) False,L (1 % 4) False])

m2 = (measures_divide_leafs [m1] (repeat 3)) !! 0
m3 = (measures_divide_leafs [m2] (repeat 3)) !! 0
