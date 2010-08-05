module LispTest where
import Lisp
import Test.HUnit

lisp1 = TestList
        [(LispInteger 1)     ~=? (LispInteger 1)
        ,"123"               ~=? (printLisp (LispInteger 123))
        ,[(LispInteger 123)] ~=? case (parseLisp "123") of
                                   Right x -> x
        ,[(LispInteger 123)] ~=? case (parseLisp " 123") of
                                   Right x -> x
        ,"left"              ~=? case (parseLisp "123(") of
                                   Left  _ -> "left"
                                   Right _ -> "right"
        ,[(LispInteger 1),
          (LispInteger 2)]   ~=? case (parseLisp "1 2") of
                                   Right x -> x
        ]
