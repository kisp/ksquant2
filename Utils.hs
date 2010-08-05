module Utils where

neighbours list = zip list (tail list)

isForAllNeighbours pred list = and (map (uncurry pred) (neighbours list))

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)
