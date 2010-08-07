module Main where

import Data.Ratio
import Data.List

import qualified Lily as L
import Input
import qualified Interval as Iv
import qualified Measure as M
import MeasureToLily
import Lisp
import SimpleFormat

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

points = Iv.ascending_intervals2points input'
groups = Iv.groupPointsByIntervalls beats_intervals points

best_divs = (map (uncurry (Iv.best_div divs)) (zip (Iv.get_ascending_intervals beats_intervals) groups))

measures' = M.measures_divide_leafs measures (map toInteger best_divs)

quant_grid = (M.measures_leaf_intervals measures')
quant_grid' = Iv.ascending_intervals (map rational_pair_to_time_pair quant_grid)
-- groups' = Iv.groupPointsByIntervalls quant_grid' points

make_qevent ivs ((start_i,end_i),e) = QEvent (Iv.start (ivs!!start_i)) (Iv.start (ivs!!end_i)) [e]

qevents = Iv.ascending_intervals (map ((make_qevent quant_grid) . (Iv.quantize_iv quant_grid')) input)

----------------

nice_show label obj = do
  putStrLn "------------"
  putStrLn $ (label ++ ":\n" ++ (show obj))

main2 = do
  nice_show "input" input
  -- nice_show "input'" input'
  -- nice_show "groups" groups
  -- nice_show "best_divs" best_divs
  nice_show "quant_grid" quant_grid
  -- nice_show "groups'" groups'
  nice_show "qevents" qevents
  L.exportLily "atest" (map m_to_lily measures')

---------------------

getSimple x = case getf x (LispKeyword "SIMPLE") of
                Just s -> s
                Nothing -> error "Could not find :simple"

main = do
  s <- getContents
  case (parseLisp s) of
    Right [s] -> (print . sexp2simpleFormat . getSimple) s
    Left err -> do { print err ; error "parse error" }
