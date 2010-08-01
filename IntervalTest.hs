module IntervalTest where
import Interval
import Test.QuickCheck

instance Arbitrary (Int,Int) where
    arbitrary     = do
      a <- choose (0, 20)
      b <- choose (0, 20)
      return (a,b)
    coarbitrary c = error ""

mytest :: (Int,Int) -> Bool
mytest iv = start iv < end iv

run = quickCheck mytest
