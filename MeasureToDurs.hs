module MeasureToDurs (vToDurs)

where

import qualified Measure as M (Ms, M, leafLRAs, mleaves, mroot, leafEffectiveDurs, tied)
import qualified Lisp as L (LispVal, toSexp, readLisp')

vToDurs :: M.Ms -> L.LispVal
vToDurs v = L.toSexp $ concatMap mToDurs v 

mToDurs :: M.M -> [L.LispVal]
mToDurs m = map L.toSexp $ concat (zipWith3 t factors durs ties)
  where factors = map (M.leafLRAs 1 (-1)) $ M.mleaves m
        durs = map (4*) (M.leafEffectiveDurs $ M.mroot m)
        ties = map M.tied $ M.mleaves m
        t :: Rational -> Rational -> Bool -> [L.LispVal]
        t a b False = [L.toSexp $ a * b]
        t a b True = [L.toSexp $ a * b, L.readLisp' ":tie"]
