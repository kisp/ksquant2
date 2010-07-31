module Main where
import Lily

m100 = [Measure 4 4 [Note (Dur D4 1) False, Note (Dur D8 0) True, Note (Dur D2 0) False],
        Measure 4 4 [Note (Dur D4 2) False, Note (Dur D16 0) False, Note (Dur D2 0) False],
        Measure 4 4 [Rest (Dur D1 0)],
        Measure 3 4 [Note (Dur D4 0) False, Note (Dur D4 0) True,
                     Times 2 3 [Note (Dur D8 0) False, Note (Dur D8 0) False, Note (Dur D8 0) True]],
        Measure 3 4 [Note (Dur D2 0) False, Note (Dur D4 0) False]]


main = do
  putStrLn "hi this is the main program"
  exportLily "foo" m100
  
