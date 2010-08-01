module IntervalTest where
import Interval
import Test.QuickCheck
import System
import Text.Printf

ivmax = 30

data TestInterval = TestInterval (Int,Int)
                    deriving Show

instance Interval TestInterval Int where
    start (TestInterval x) = start x
    end (TestInterval x) = end x

instance Arbitrary TestInterval where
    arbitrary     = do
      a <- choose (0, ivmax-1)
      b <- choose (a+1, ivmax)
      return (TestInterval (a,b))

prop_good_iv :: TestInterval -> Bool
prop_good_iv iv = start iv < end iv

prop_isPointInInterval :: TestInterval -> Bool
prop_isPointInInterval iv = any (isPointInInterval iv) [0..ivmax]

prop_intersect :: TestInterval -> TestInterval -> Bool
prop_intersect a b = intersect a b == safe_intersect a b
    where safe_intersect a b = any inBoth [0..ivmax]
          inBoth x = isPointInInterval a x && isPointInInterval b x

prop_isStrictlyAfter :: TestInterval -> TestInterval -> Bool
prop_isStrictlyAfter a b = isStrictlyAfter a b == safeisStrictlyAfter a b
    where safeisStrictlyAfter a b = not (intersect a b) && start b >= end a

good :: Result -> Bool
good Success {} = True
good NoExpectedFailure {} = True
good _ = False

runtest name test = do
  printf "%-25s: " name
  r <- quickCheckResult test
  if not (good r) then exitFailure else return ()

main = do
  runtest "prop_good_iv" prop_good_iv
  runtest "prop_isPointInInterval" prop_isPointInInterval
  runtest "prop_intersect" prop_intersect
  runtest "prop_isStrictlyAfter" prop_isStrictlyAfter
