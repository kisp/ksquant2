{-# LANGUAGE FlexibleContexts #-}

module MeasureToDurs (vToDurs)

where

import qualified Measure as M (Ms, M, leafLRAs, mleaves, mroot, leafEffectiveDurs, tied, enotes)
import qualified Lisp as L (LispVal, toSexp, readLisp', n60)

tie :: L.LispVal
tie = L.readLisp' ":tie"

vToDurs :: M.Ms -> L.LispVal
vToDurs v = L.toSexp $ concatMap mToDurs v 

mToDurs :: M.M -> [L.LispVal]
mToDurs m = map L.toSexp $ concat (zipWith3 t (zip factors durs) ties notes)
  where factors = map (M.leafLRAs (1 :: Integer) (-1)) $ M.mleaves m
        durs = map (4*) (M.leafEffectiveDurs $ M.mroot m)
        ties = map M.tied $ M.mleaves m
        notes = map M.enotes $ M.mleaves m
        t (1, b) False ns | ns == L.n60 = [L.toSexp b]
                          | otherwise = [L.toSexp [L.toSexp b, L.readLisp' ":notes", ns]]
        t (1, b) True ns | ns == L.n60 = [L.toSexp b, tie]
                         | otherwise = [L.toSexp [L.toSexp b, L.readLisp' ":notes", ns], tie]
        t (-1, b) False _ = [L.toSexp $ (-1) * b]
        t (-1, _) True _ = error "a rest should not have a tie"
        t (_, _) _ _ = error "not -1 or 1 should not happen for fst of first arg"
