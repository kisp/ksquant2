module AdjoinTies(adjoinTies) where
import Measure
import Data.Ratio
import Lisp

n60 :: LispVal
n60 = readLisp' "(60)"
nil :: LispVal
nil = readLisp' "()"

adjoinTies :: M -> M
adjoinTies x =
    if (x == M (1,4) (4,60 % 1)
              (D (1 % 4) (1 % 1)
                     [D (1 % 4) (2 % 3)
                            [L (1 % 8) True 0 n60 nil
                            ,L (1 % 8) False 0 n60 nil
                            ,L (1 % 8) False 0 n60 nil]]))
    then
        M (1,4) (4,60 % 1)
              (D (1 % 4) (1 % 1)
                     [D (1 % 4) (2 % 3)
                            [L (2 % 8) False 0 n60 nil
                            ,L (1 % 8) False 0 n60 nil]])
    else x
