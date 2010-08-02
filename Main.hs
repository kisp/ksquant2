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

ms = take 2 (repeat (M.m (4,4) 60 (M.l (4%4) False))) ++
     take 2 (repeat (M.m (3,4) 60 (M.l (3%4) False)))

-- ms3 = (fst (M.smap (M.transform_leafs' mytransform) ms (repeat 4)))
ms4 = ms ++ (M.measures_divide_leafs ms [2..20])

main = do
  putStrLn "Here we have some sample input:"
  putStrLn $ show ivs
  putStrLn $ show points
  putStrLn $ show groups
  putStrLn $ show ms
  putStrLn $ show (map m_to_lily ms4)
  L.exportLily "atest" (map m_to_lily ms4)
  -- putStrLn $ show (Iv.divide_interval ((Iv.get_ascending_intervals ms) !! 0) 4)
