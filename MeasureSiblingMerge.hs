{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module MeasureSiblingMerge (measureSiblingMerge)

where

import qualified Measure as M ( M(..), E(..), m, d, r, l)

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys
converge _ _ = error "converge"

measureSiblingMerge :: M.M -> M.M
measureSiblingMerge (M.M ts metro e) = r
  where r = M.m ts metro $ (converge (==) . iterate eltSiblingMerge) e

eltSiblingMerge :: M.E -> M.E
eltSiblingMerge l@M.L{} = l
eltSiblingMerge r@M.R{} = r
eltSiblingMerge (M.D dur factor children) =
  M.d dur factor $ foldr mcons [] merged_children
  where merged_children = map eltSiblingMerge children

mcons :: M.E -> [M.E] -> [M.E]

mcons (M.R dur _) (M.R dur' _ : es) = M.r (dur + dur'):es

mcons (M.L dur True _ ns exps) (M.L dur' tie _ _ _ : es) = M.l (dur + dur') tie ns exps : es

mcons l@M.L{} es = l:es
mcons r@(M.R _ _) es = r:es

mcons (M.D dur factor children) (M.D dur' factor' children' : es)
  | factor == factor' = M.d (dur + dur') factor (children ++ children') : es

mcons a b = a:b
