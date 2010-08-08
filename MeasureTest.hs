module MeasureTest where
import Measure
import Test.HUnit
import Data.Ratio

measure1 = TestList
           [[M (4,4) (60 % 1)
             (D (1 % 1) (1 % 1)
              [L (1 % 4) False,
               L (1 % 4) False,
               L (1 % 4) False,
               L (1 % 4) False])]
            ~=?  measures_with_beats [(4,4)] [60]
           ,[M (3,4) (60 % 1)
             (D (3 % 4) (1 % 1)
              [L (1 % 4) False,
               L (1 % 4) False,
               L (1 % 4) False])]
            ~=?  measures_with_beats [(3,4)] [60]
           ,[M (5,8) (60 % 1)
             (D (5 % 8) (1 % 1)
              (take 5 (repeat (L (1 % 8) False))))]
            ~=?  measures_with_beats [(5,8)] [60]
           ]
