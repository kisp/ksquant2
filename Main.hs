module Main where
import qualified Lily as L
import Input
import qualified Interval as Iv

import qualified Measure as M
import MeasureToLily

import Data.Ratio

----------------

rational_to_time :: Rational -> Time
rational_to_time x = fromRational x

rational_pair_to_time_pair (x,y) = (rational_to_time x, rational_to_time y)

----------------

input = [Event 0 0.3333333, Event 1.25 2.25, Event 2.25 4, Event 5 7.33333333]
measures = M.measures_with_beats (take 4 (repeat (4,4))) (repeat 60)
divs = [1..8] :: [Int]
----------------

input' = Iv.ascending_intervals input

beats_intervals = Iv.ascending_intervals (map rational_pair_to_time_pair (M.measures_leaf_intervals measures))

points = (Iv.ascending_intervals2points input')
groups = Iv.groupPointsByIntervalls beats_intervals points

best_divs = (map (uncurry (Iv.best_div divs)) (zip (Iv.get_ascending_intervals beats_intervals) groups))

measures' = M.measures_divide_leafs measures (map toInteger best_divs)

----------------

main = do
  putStrLn "Here we have some sample input:"
  putStrLn $ show groups
  putStrLn $ show best_divs
  L.exportLily "atest" (map m_to_lily measures')
