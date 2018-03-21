{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module MeasureSiblingMerge (measureSiblingMerge)

where

import Measure ( M(..), E(..), m, d, r, l)

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys
converge _ _ = error "converge"

measureSiblingMerge :: M -> M
measureSiblingMerge (M ts metro e) = r
  where r = m ts metro $ (converge (==) . iterate eltSiblingMerge) e

eltSiblingMerge :: E -> E
eltSiblingMerge l@L{} = l
eltSiblingMerge r@R{} = r
eltSiblingMerge (D dur factor children) =
  d dur factor $ foldr mcons [] merged_children
  where merged_children = map eltSiblingMerge children

mcons :: E -> [E] -> [E]

mcons (R dur _) (R dur' _ : es) = r (dur + dur'):es

mcons (L dur True _ ns exps) (L dur' tie _ _ _ : es) = l (dur + dur') tie ns exps : es

mcons l@L{} es = l:es
mcons r@(R _ _) es = r:es

mcons (D dur factor children) (D dur' factor' children' : es)
  | factor == factor' = d (dur + dur') factor (children ++ children') : es

mcons a b = a:b
