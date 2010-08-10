module IntervalTest where
import Interval
import Test.QuickCheck
import Data.List (delete,sort,nub)

ivmax = 30::Int
domain = [0..ivmax]

data TestInterval = TestInterval (Int,Int)
                    deriving Show

instance Interval TestInterval Int where
    start (TestInterval x) = start x
    end (TestInterval x) = end x

instance Point Int Int where
    point x = x

instance Arbitrary TestInterval where
    arbitrary     = do
      a <- choose (0, ivmax-1)
      b <- choose (a+1, ivmax)
      return (TestInterval (a,b))

prop_good_iv :: TestInterval -> Bool
prop_good_iv iv = start iv < end iv

prop_isPointInInterval :: TestInterval -> Bool
prop_isPointInInterval iv = any (isPointInInterval iv) domain

prop_intersect :: TestInterval -> TestInterval -> Bool
prop_intersect a b = intersect a b == safe_intersect a b
    where safe_intersect _ _ = any inBoth domain
          inBoth x = isPointInInterval a x && isPointInInterval b x

prop_isStrictlyAfter :: TestInterval -> TestInterval -> Bool
prop_isStrictlyAfter a b = isStrictlyAfter a b == safeisStrictlyAfter a b
    where safeisStrictlyAfter a b = not (intersect a b) && start b >= end a

filteredDomain2Intervalls [] = []
filteredDomain2Intervalls (x:xs) = map TestInterval (r xs x x)
    where r [] start last = [(start,last+1)]
          r (x:xs) start last | (x == last + 1) = r xs start x
                                | otherwise = (start,last+1) : (r xs x x)

delete_random list = do
  pos <- choose (0, (length list) - 1)
  return (delete (list !! pos) list)

monad_repeat :: (Monad m) => Int -> m a -> (a -> m a) -> m a
monad_repeat 0 x _ = x
monad_repeat n x fn = monad_repeat (n-1) (x >>= fn) fn

instance Arbitrary (AscendingIntervals TestInterval) where
    arbitrary     = do
      n <- choose(0,ivmax)
      list <- monad_repeat n (return domain) delete_random
      return (ascending_intervals (filteredDomain2Intervalls list))

instance Arbitrary (AscendingPoints Int) where
    arbitrary     = do
      n <- choose(0,ivmax)
      list <- monad_repeat n (return domain) delete_random
      return (ascending_points list)

prop_groupPointsByIntervalls :: AscendingIntervals TestInterval -> AscendingPoints Int -> Bool
prop_groupPointsByIntervalls ivs xs = (groupPointsByIntervalls ivs xs) == (safe_groupPointsByIntervalls ivs xs)
    where safe_groupPointsByIntervalls ivs _ = map grap (get_ascending_intervals ivs)
          grap iv = ascending_points (filter (isPointInInterval iv) (get_ascending_points xs))

mt ivs xs = (safe_groupPointsByIntervalls ivs xs)
    where safe_groupPointsByIntervalls ivs _ = map grap (get_ascending_intervals ivs)
          grap iv = ascending_points (filter (isPointInInterval iv) (get_ascending_points xs))

prop_ascending_intervals2points :: AscendingIntervals TestInterval -> Bool
prop_ascending_intervals2points ivs = (ascending_intervals2points ivs) == (safe_ascending_intervals2points ivs)
    where safe_ascending_intervals2points ivs = let ivs' = get_ascending_intervals ivs
                                                in (ascending_points . sort . nub) ((map start ivs') ++ (map end ivs'))
