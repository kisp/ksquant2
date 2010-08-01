module IntervalTest where
import Interval
import Test.QuickCheck

data TestInterval = TestInterval (Int,Int)
                    deriving Show

instance Interval TestInterval Int where
    start (TestInterval x) = start x
    end (TestInterval x) = end x

instance Arbitrary TestInterval where
    arbitrary     = do
      a <- choose (0, 30)
      b <- choose (a+1, 31)
      return (TestInterval (a,b))

mytest :: TestInterval -> Bool
mytest iv = start iv < end iv

main = do
  quickCheck mytest
