module Internal where

data Measure = Measure (Int, Int) Float
               deriving Show

type Measures = [Measure]

foo1 = [Measure (4,4) 72]
