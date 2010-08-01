module Interval (Interval
                ,start
                ,end
                ,intersect
                ,isPointInInterval
                ,isStrictlyAfter
                ,ascending_intervals) where
import Utils

-- http://www.haskell.org/haskellwiki/Functional_dependencies
-- This tells Haskell that b is uniquely determined from a. 
class (Num b) => Interval a b | a -> b where
    start :: a -> b
    end :: a -> b
    dur :: a -> b
    dur x = end x - start x
    end x = start x + dur x

instance (Num t) => Interval (t,t) t where
    start (x,_) = x
    end (_,x) = x

-- Do the intervals a and b have common points?
intersect a b =
    let s1 = start a
        e1 = end a
        s2 = start b
        e2 = end b
    in
      if not (s1 <= s2) then
          intersect b a
      else
          s2 < e1

-- Is x in iv?
isPointInInterval iv x = start iv <= x && x < end iv

isStrictlyAfter a b = start b >= end a

data AscendingIntervals a = AscendingIntervals [a]
                            deriving Show

ascending_intervals ivs =
    if not (isForAllNeighbours isStrictlyAfter ivs) then
        error "not (isForAllNeighbours isStrictlyAfter ivs)"
    else
        AscendingIntervals ivs
