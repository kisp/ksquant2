module MeasureToEnp where
import qualified Measure as M
import qualified Enp as E
import Data.Ratio

durs2factor xs = (foldl1 lcm (map denominator xs)) % 1
ratio2integer r | denominator r == 1 = numerator r

e_to_enp :: Rational -> M.E -> E.Elt
e_to_enp f (M.L d tie) = E.Chord (ratio2integer (d * f))
e_to_enp f (M.R d) = E.Rest (ratio2integer (d * f))
e_to_enp f (M.D d r es) =
    let f' = durs2factor (map M.dur es)
    in E.Div (ratio2integer (d * f))
           (map (e_to_enp f') es)

unwrap (E.Div _ e) = e

wrapIfNeeded e@(E.Div _ _) = e
wrapIfNeeded x = (E.Div 1 [x])

adaptForTimesig (n,_) es = let f = n `div` (sum (map E.dur es))
                           in map (E.scaleElt f) es

m_to_enp :: M.M -> E.Measure
m_to_enp (M.M ts t e) =
    let f = ((denominator (M.dur e))%1)
        list = (unwrap (wrapIfNeeded (e_to_enp f e)))
        list' = map wrapIfNeeded list
        list'' = adaptForTimesig ts list'
    in (E.makeMeasure ts list'')
