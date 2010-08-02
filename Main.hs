module Main where
import qualified Lily as L
import Input
import qualified Internal as I
import qualified Interval as Iv

i1 = [Event 0 0.25, Event 1.25 2.25, Event 2.25 4, Event 5 7.25]
ivs = (Iv.ascending_intervals i1)
points = (Iv.ascending_intervals2points ivs)
groups = Iv.groupPointsByIntervalls ivs points

test = L.exportLily "foo" L.m1


ms = I.measures_from_timesigs_tempos (take 5 (repeat (4,4))) (repeat 60)

main = do
  putStrLn "Here we have some sample input:"
  putStrLn $ show ivs
  putStrLn $ show points
  putStrLn $ show groups
  putStrLn $ show ms
