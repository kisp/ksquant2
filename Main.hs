module Main where
import qualified Lily as L
import Input
import qualified Internal as I
import qualified Interval as Iv

i1 = [Event 0 0.25, Event 1.25 2.25, Event 2.25 4, Event 5 7.25]

main = do
  putStrLn "Here we have some sample input:"
  putStrLn $ show (Iv.ascending_intervals i1)
