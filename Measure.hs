module Measure (m
               ,l
               ,E(L,D)
               ,M(M)
               ,Rat)
where
import Data.Ratio

type Rat = Ratio Int

isPowerOfTwo 1 = True
isPowerOfTwo x | x > 1 = if (even x) then
                             isPowerOfTwo (x `div` 2)
                         else
                             False

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
       | L Rat
       deriving Show

dur (D d _ _) = d
dur (L d)     = d

timesig_dur (n,d) = (n%d)

m timesig tempo d = if not(check timesig tempo d) then
                        error "m timesig tempo d not valid"
                    else M timesig tempo d
    where check timesig tempo d = (dur d) == (timesig_dur timesig)

l d = if not(check d) then
          error "l d not valid"
      else L d
    where check d = notableDur d
