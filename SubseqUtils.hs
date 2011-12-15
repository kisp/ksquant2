-- This file is part of KSQuant2.

-- Copyright (c) 2010 - 2011, Kilian Sprotte. All rights reserved.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module SubseqUtils where
import Data.List (find)

allIndexPairs n = r 0 n
    where r x y | x >= n = []
                | x == y = r (x + 1) n
                | otherwise = (x,y) : r x (y - 1)

type Pred a = (a -> Bool)
type LispPred a = ([a] -> Bool)
type GroupFinder a = [a] -> Maybe ([a], ([a], [a]))
type AllFinder a = [a] -> ([[a]], [[a]])

groupFinder :: LispPred a -> GroupFinder a
groupFinder p xs = let explode (a,b) = (take l (drop a xs),(take a xs, drop l (drop a xs)))
                         where l = b - a
                   in find (p . fst) (map explode (allIndexPairs (length xs)))

allFinder :: GroupFinder a -> AllFinder a
allFinder finder xs = let r [] = ([],[])
                          r xs = case finder xs of
                            Nothing -> ([],[xs])
                            Just (group,(prefix,suffix)) ->
                              let (groups,antigroups) = r suffix
                              in (group:groups,prefix:antigroups)
                      in r xs

rejoin :: [[a]] -> [[a]] -> [a]
rejoin [] [a] = a
rejoin (g:gs) (a:as) = a ++ g ++ rejoin gs as

findGroup :: Pred a -> GroupFinder a
findGroup p = groupFinder (all p)

findGroups :: Pred a -> AllFinder a
findGroups = allFinder . findGroup

findGroup3 :: Pred a -> Pred a -> Pred a -> GroupFinder a
findGroup3 s m e =
    groupFinder ((s.head) `conjoin`
                 (all m . middle) `conjoin`
                 (e.last))
    where middle = tail . init

findGroups3 :: Pred a -> Pred a -> Pred a -> AllFinder a
findGroups3 s m e = allFinder (findGroup3 s m e)

class Predicate a where
  complement :: a -> a
  disjoin    :: a -> a -> a
  conjoin    :: a -> a -> a

instance Predicate Bool where
  complement = not
  disjoin    = (||)
  conjoin    = (&&)

instance (Predicate b) => Predicate (a -> b) where
  complement = (complement .)
  disjoin f g x = f x `disjoin` g x
  conjoin f g x = f x `conjoin` g x
