module Measure (m
               ,l
               ,d
               ,r
               ,E(L,D,R)
               ,M(M)
               ,Rat
               ,transform_leafs
               ,transform_leafs'
               ,smap
               ,measures_divide_leafs)
where
import Data.Ratio

type Rat = Ratio Int

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
div_to_ratio d = ((lowerPowerOfTwo d) % d)

notableDur :: Rat -> Bool
notableDur x = h (numerator x) (denominator x)
    where h 1 d = isPowerOfTwo d
          h 3 d = isPowerOfTwo d && d >= 2
          h 7 d = isPowerOfTwo d && d >= 4
          -- 15 = 1 + 2 + 4 + 8
          h 15 d = isPowerOfTwo d && d >= 8
          h _ _ = False

type Timesig = (Int,Int)
type Tempo = Rat

data M = M Timesig Tempo E
       deriving Show

data E = D Rat Rat [E]
       | L Rat Bool
       | R Rat
       deriving Show

dur (D d _ _) = d
dur (L d _)     = d
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
        where trans :: E -> [Int] -> (E, [Int])
              trans (L dur tie) (d':ds) =
                  let n = d'
                      r = div_to_ratio n
                  in (d dur r (take n (repeat (l ((dur/(n%1)/r)) False))),
                        ds)
              trans r@(R dur) ds = (r,ds)
