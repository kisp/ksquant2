module Interval (Point
                ,point
                ,Interval
                ,start
                ,end                
                ,intersect
                ,isPointInInterval
                ,isStrictlyAfter
                ,ascending_intervals
                ,get_ascending_intervals
                ,AscendingIntervals --only type not constructor
                ,ascending_points
                ,get_ascending_points
                ,AscendingPoints --only type not constructor
                ,groupPointsByIntervalls
                ,ascending_intervals2points
                ) where
import Utils

-- http://www.haskell.org/haskellwiki/Functional_dependencies
-- This tells Haskell that b is uniquely determined from a. 
class (Num b) => Interval a b | a -> b where
    start :: a -> b
    end :: a -> b
    dur :: a -> b
    -- defaults
    dur x = end x - start x
    end x = start x + dur x

class Point a b | a -> b where
    point :: a -> b

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
isPointInInterval iv x = start iv <= point x && point x < end iv

isStrictlyAfter a b = start b >= end a

data AscendingIntervals a = AscendingIntervals [a]
                            deriving Show

isAscending_intervals ivs = isForAllNeighbours isStrictlyAfter ivs

ascending_intervals ivs =
    if not (isAscending_intervals ivs) then
        error "not (isForAllNeighbours isStrictlyAfter ivs)"
    else
        AscendingIntervals ivs

get_ascending_intervals (AscendingIntervals xs) = xs

data AscendingPoints a = AscendingPoints [a]
                       deriving (Show, Eq)
ascending_points xs =
    if not (isForAllNeighbours (<) xs) then
        error "not (isForAllNeighbours (<) xs)"
    else
        AscendingPoints xs

get_ascending_points (AscendingPoints xs) = xs

-- for each interval return ascending_points that are all the points
-- from xs contained in the interval
groupPointsByIntervalls ivs xs = f (get_ascending_intervals ivs) (get_ascending_points xs)
    where f [] _ = []
          f (iv:ivs) [] = (ascending_points []) : (f ivs [])
          f (iv:ivs) (x:xs)
              | (point x) < (start iv) = f (iv:ivs) xs
              | otherwise = (ascending_points (takeWhile (<(end iv)) (x:xs))) :
                            (f ivs (dropWhile (<(end iv)) (x:xs)))

-- TODO call internal Constructor instead of safe ascending_points
ascending_intervals2points ivs = ascending_points (f (get_ascending_intervals ivs))
    where f (iv:ivs) = [start iv,end iv] ++ (g ivs (end iv))
          g [] _ = []
          g (iv:ivs) last = if (start iv == last) then
                                [end iv] ++ (g ivs (end iv))
                            else
                                [start iv,end iv] ++ (g ivs (end iv))
