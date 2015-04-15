module AdjoinTiesTest where
import Measure
import Test.HUnit
import Data.Ratio
import Lisp
import AdjoinTies

n60 :: LispVal
n60 = readLisp' "(60)"
nil :: LispVal
nil = readLisp' "()"

adjoin1 :: Assertion
adjoin1 =
            M (4,4) (4,60 % 1)
            (D (1 % 1) (1 % 1)
             [l (1 % 4) False n60 nil,
              l (1 % 4) False n60 nil,
              l (1 % 4) False n60 nil,
              l (1 % 4) False n60 nil])
            @=?
            adjoinTies
            (M (4,4) (4,60 % 1)
             (D (1 % 1) (1 % 1)
              [l (1 % 4) False n60 nil,
               l (1 % 4) False n60 nil,
               l (1 % 4) False n60 nil,
               l (1 % 4) False n60 nil]))
