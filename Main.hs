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

ms = [M.m (2,4) 60 (M.l (2%4))
     ,M.m (4,4) 60 (M.d (4%4) 1 [M.d (1%4) 1 [M.l (1%8)
                                             ,M.l (1%8)]
                                ,M.d (1%4) (2%3) [M.l (1%8)
                                                 ,M.l (1%8)
                                                 ,M.l (1%8)]
                                ,M.l (2%4)])]

main = do
  putStrLn "Here we have some sample input:"
  putStrLn $ show ivs
  putStrLn $ show points
  putStrLn $ show groups
  putStrLn $ show ms
  putStrLn $ show (map m_to_lily ms)
  L.exportLily "atest" (map m_to_lily ms)
  -- putStrLn $ show (Iv.divide_interval ((Iv.get_ascending_intervals ms) !! 0) 4)
