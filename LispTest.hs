module LispTest where
import Lisp
import Test.HUnit

rightOrError x = case x of
                   Right y -> y
                   Left _ -> error "rightOrError"

lisp1 = TestList
        [(LispInteger 1)     ~=? (LispInteger 1)
        ,"123"               ~=? (printLisp (LispInteger 123))
        ,[(LispInteger 123)] ~=? rightOrError (parseLisp "123")
        ,[(LispInteger 123)] ~=? rightOrError (parseLisp " 123")
        ,"left"              ~=? case (parseLisp "123(") of
                                   Left  _ -> "left"
                                   Right _ -> "right"
        ,[(LispInteger 1),
          (LispInteger 2)]   ~=? rightOrError (parseLisp "1 2")
        ]

lisp2 = TestList
        [
         [(LispFloat 123.12)] ~=? rightOrError (parseLisp "123.12")
        ,[(LispKeyword "FOO")] ~=? rightOrError (parseLisp ":FOO")
        ,[(LispKeyword "BAR")] ~=? rightOrError (parseLisp ":bar")
        ,[(LispKeyword "F/123")] ~=? rightOrError (parseLisp ":f/123")
        ,[(LispList [LispInteger 1])] ~=?
         rightOrError (parseLisp "(1)")
        ,[(LispList [LispInteger 1, LispInteger 2])] ~=?
         rightOrError (parseLisp "(1 2)")
        ,[(LispList [])] ~=?
         rightOrError (parseLisp "()")
        ,[(LispList [LispList [LispInteger 1]])] ~=?
         rightOrError (parseLisp "((1))")
        ,[(LispInteger (-123))] ~=?
         rightOrError (parseLisp "-123")
        ,[(LispFloat (-123.12))] ~=?
         rightOrError (parseLisp "-123.12")
        ,[(LispInteger 1)] ~=?
         rightOrError (parseLisp "1 ")
        ,[(LispFloat 0.5)] ~=?
         rightOrError (parseLisp "1/2")
        ,[(LispFloat (-0.1))] ~=?
         rightOrError (parseLisp "-1/10")
        ,[(LispSymbol "T")] ~=?
         rightOrError (parseLisp "t")
        ]

lisp3 = TestList
        [
         "(:FDS 1 2.3)" ~=?
         printLisp (LispList [LispKeyword "FDS",LispInteger 1,LispFloat 2.3])
        ,"()" ~=?
         printLisp (LispList [])
        ,"T" ~=?
         printLisp (LispSymbol "T")
        ,True ~=?
         propertyListP (LispList [LispKeyword "FDS",LispInteger 1])
        ,False ~=?
         propertyListP (LispList [LispKeyword "FDS"])
        ,False ~=?
         propertyListP (LispList [LispInteger 1, LispInteger 2])
        ,False ~=?
         propertyListP (LispInteger 1)
        ,(LispInteger 1) ~=?
         case (getf (LispList [LispKeyword "FDS",LispInteger 1]) (LispKeyword "FDS")) of
           Just x -> x
           Nothing -> error "Nothing"
        ,Nothing ~=?
         getf (LispList [LispKeyword "FDS",LispInteger 1]) (LispKeyword "BAR")
        ]
