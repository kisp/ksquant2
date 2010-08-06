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

lisp2 = TestList
        [
         [(LispFloat 123.12)] ~=? case (parseLisp "123.12") of
                                   Right x -> x
        ,[(LispKeyword "FOO")] ~=? case (parseLisp ":FOO") of
                                   Right x -> x
        ,[(LispKeyword "BAR")] ~=? case (parseLisp ":bar") of
                                   Right x -> x
        ,[(LispList [LispInteger 1])] ~=?
         case (parseLisp "(1)") of
           Right x -> x
        ,[(LispList [LispInteger 1, LispInteger 2])] ~=?
         case (parseLisp "(1 2)") of
           Right x -> x
        ,[(LispList [])] ~=?
         case (parseLisp "()") of
           Right x -> x
        ,[(LispList [LispList [LispInteger 1]])] ~=?
         case (parseLisp "((1))") of
           Right x -> x
        ,[(LispInteger (-123))] ~=?
         case (parseLisp "-123") of
           Right x -> x
        ,[(LispFloat (-123.12))] ~=?
         case (parseLisp "-123.12") of
           Right x -> x
        ,[(LispInteger 1)] ~=?
         case (parseLisp "1 ") of
           Right x -> x
        ]

lisp3 = TestList
        [
         "(:FDS 1 2.3)" ~=?
         printLisp (LispList [LispKeyword "FDS",LispInteger 1,LispFloat 2.3])
        ,"()" ~=?
         printLisp (LispList [])
        ]
