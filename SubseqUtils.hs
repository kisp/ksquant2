module SubseqUtils where
import Data.List (find)

allIndexPairs n = r 0 n
    where r x y | x >= n = []
		| x == y = r (x + 1) n
		| otherwise = (x,y) : (r x (y - 1))

type GroupFinder a = (a -> Bool) -> [a] -> Maybe ([a], ([a], [a]))
type AllFinder a = (a -> Bool) -> [a] -> ([[a]], [[a]])

singleCond :: GroupFinder a
singleCond f xs = find test (map explode (allIndexPairs (length xs)))
    where test (l,_) = all f l
	  explode (a,b) = (take l (drop a xs),(take a xs, drop l (drop a xs)))
	      where l = b - a

allFinder :: GroupFinder a -> AllFinder a
allFinder f = \p xs ->
	      let finder = f p
		  r [] = ([],[])
		  r xs = case finder xs of
			   Nothing -> ([],[xs])
			   Just (group,(prefix,suffix)) ->
			       let (groups,antigroups) = r suffix
			       in ((group:groups),(prefix:antigroups))
	      in r xs

rejoin :: [[a]] -> [[a]] -> [a]
rejoin [] [a] = a
rejoin (g:gs) (a:as) = a ++ g ++ (rejoin gs as)
