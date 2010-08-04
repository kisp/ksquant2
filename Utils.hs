module Utils where

neighbours list = zip list (tail list)

isForAllNeighbours pred list = and (map (uncurry pred) (neighbours list))

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

binsearch xs value low high
   | high < low       = Nothing
   | xs!!mid > value  = binsearch xs value low (mid-1)
   | xs!!mid < value  = binsearch xs value (mid+1) high
   | otherwise        = Just mid
   where
   mid = low + ((high - low) `div` 2)
