module Main where
import qualified Lily as L
import Input
import qualified Internal as I
import qualified Interval as Iv

import qualified Measure as M
import MeasureToLily

import Data.Ratio

i1 = [Event 0 0.25, Event 1.25 2.25, Event 2.25 4, Event 5 7.25]
ivs = (Iv.ascending_intervals i1)
points = (Iv.ascending_intervals2points ivs)
groups = Iv.groupPointsByIntervalls ivs points

test = L.exportLily "foo" L.m1

-- ms = I.measures_from_timesigs_tempos (take 5 (repeat (4,4))) (repeat 72)

ms2 = [M.m (2,4) 60 (M.l (2%4) False)
     ,M.m (4,4) 60 (M.d (4%4) 1 [M.d (1%4) 1 [M.l (1%8) False
                                             ,M.r (1%8)]
                                ,M.d (1%4) (2%3) [M.l (1%8) False
                                                 ,M.l (1%8) False
                                                 ,M.l (1%8) True]
                                ,M.l (2%4) False])]

ms = take 4 (repeat (M.m (4,4) 60 (M.l (4%4) False)))

div_to_ratio 1 = 1
div_to_ratio 2 = 1
div_to_ratio 3 = 2%3
div_to_ratio 4 = 1
div_to_ratio 5 = 4%5
div_to_ratio 6 = 4%6
div_to_ratio 7 = 4%7
div_to_ratio 8 = 1
div_to_ratio 9 = 8%9
div_to_ratio 10 = 8%10
div_to_ratio 11 = 8%11
div_to_ratio 12 = 8%12
div_to_ratio 13 = 8%13

mytransform (M.L dur tie) = let n = 5
                                r = div_to_ratio n
                            in M.d dur r (take n (repeat (M.l ((dur/(n%1)/r))
                                                           False)))
mytransform r@(M.R dur) = r

ms3 = ms2 ++ (map (M.transform_leafs mytransform) ms2)

main = do
  putStrLn "Here we have some sample input:"
  putStrLn $ show ivs
  putStrLn $ show points
  putStrLn $ show groups
  putStrLn $ show ms
  putStrLn $ show (map m_to_lily ms3)
  L.exportLily "atest" (map m_to_lily ms3)
  -- putStrLn $ show (Iv.divide_interval ((Iv.get_ascending_intervals ms) !! 0) 4)
