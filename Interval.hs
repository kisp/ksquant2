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
                ,divide_interval
                ,best_div
                ,locate_point
                ,quantize_iv
                ) where
import Utils
import Data.List (sortBy)

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

instance (Num t) => Point t t where
    point x = x

-- |Do the intervals a and b have common points?
intersect a b =
    let s1 = start a
        e1 = end a
        s2 = start b
        -- e2 = end b
    in
      if not (s1 <= s2) then
          intersect b a
      else
          s2 < e1

-- |Is x in iv?
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

-- |For each interval return ascending_points that are all the points
-- from xs contained in the interval
groupPointsByIntervalls ivs xs = f (get_ascending_intervals ivs) (get_ascending_points xs)
    where f [] _ = []
          f (_:ivs) [] = (ascending_points []) : (f ivs [])
          f (iv:ivs) (x:xs)
              | (point x) < (start iv) = f (iv:ivs) xs
              | otherwise = (ascending_points (takeWhile (<(end iv)) (x:xs))) :
                            (f ivs (dropWhile (<(end iv)) (x:xs)))

-- TODO call internal Constructor instead of safe ascending_points
ascending_intervals2points ivs = ascending_points (f (get_ascending_intervals ivs))
    where f (iv:ivs) = [start iv,end iv] ++ (g ivs (end iv))
          f [] = []
          g [] _ = []
          g (iv:ivs) last = if (start iv == last) then
                                [end iv] ++ (g ivs (end iv))
                            else
                                [start iv,end iv] ++ (g ivs (end iv))

divide_interval iv n =
    let new_dur = (dur iv) / n
        points = map ((+(start iv)) . (*new_dur)) [0..n]
    in ascending_intervals (neighbours points)

-- min cost to move x to start or end of iv
boundary_move_cost iv x = let x' = point x
                              dist = min (abs ((start iv) - x')) ((abs (end iv) - x'))
                          in dist * dist

-- what is the cost of dividing iv by div (e.g. 3) and moving points
-- in xs accordingly
div_cost iv xs div = let small_ivs = divide_interval iv div
                         point_groups = groupPointsByIntervalls small_ivs xs
                         group_cost (small_iv,points) =
                             sum (map (boundary_move_cost small_iv) (get_ascending_points points))
                     in sum (map group_cost (zip (get_ascending_intervals small_ivs) point_groups))

-- return a list of pairs (div,cost). Best pair comes first. In case of
-- two identical costs the div that comes first in divs will be
-- prefered (sortBy is a stable sorting algorithm).
ranked_divs iv xs divs = sortBy test (zip divs (map (div_cost iv xs) divs))
    where test (_,a) (_,b) = compare a b 

-- choose the best div from divs
best_div divs iv xs = (round (fst (head (ranked_divs iv xs (map intToFloat divs))))) :: Int

-- TODO implement this as a binary search
locate_point ivs x = r (get_ascending_intervals ivs) (point x) 0
    where r (iv:ivs) x index
              | isPointInInterval iv x ||
                ((ivs == []) && (x >= end iv)) = (iv,(index,index+1))
              | otherwise = r ivs x (index+1)
          r a b c = error $ "locate_point " ++ show a ++ " " ++ show b ++ " " ++ show c

quantize_iv ivs iv = let s = start iv
                         e = end iv
                         ((s0,s1),(si0,si1)) = locate_point ivs s
                         ((e0,e1),(ei0,ei1)) = locate_point ivs e
                     in (snd (head (sortBy
                                   test [(cost ds de,(snd a,snd b)) |
                                         a <- [(s0,si0),(s1,si1)],
                                         b <- [(e0,ei0),(e1,ei1)],
                                         not (snd a == snd b),
                                         let ds = abs (s - (fst a)),
                                         let de = abs (e - (fst b))])),
                         iv)
    where cost ds de = abs (de - ds) + abs ds + abs de
          test a b = (fst a) `compare` (fst b)
