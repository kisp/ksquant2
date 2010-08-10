module MeasureToEnp where
import qualified Measure as M
import qualified Enp as E
import Data.Ratio

durs2factor xs = (foldl1 lcm (map denominator xs)) % 1
ratio2integer r | denominator r == 1 = numerator r
                | otherwise = error "ratio2integer: not an integer"

tied_over_from_last _ 0 = False
tied_over_from_last assoc id =
    case lookup (id-1) assoc of
      Nothing -> error "tied_over_from_last not found"
      Just x -> has_forward_tie x
    where has_forward_tie (M.R _ _) = False
          has_forward_tie (M.L _ tie _ _) = tie
          has_forward_tie (M.D _ _ _) = error "tied_over_from_last (M.D _ _ _)"

e_to_enp :: [(M.Label, M.E)] -> Rational -> M.E -> E.Elt
e_to_enp assoc f (M.L d _ id notes) = E.Chord (ratio2integer (d * f)) (tied_over_from_last assoc id) notes
e_to_enp _ f (M.R d _) = E.Rest (ratio2integer (d * f))
e_to_enp assoc f (M.D d _ es) =
    let f' = durs2factor (map M.dur es)
    in E.Div (ratio2integer (d * f))
           (map (e_to_enp assoc f') es)

unwrap (E.Div _ e) = e
unwrap _ = error "unwrap: not a Div"

wrapIfNeeded e@(E.Div _ _) = e
wrapIfNeeded x = (E.Div 1 [x])

adaptForTimesig (n,_) es = let f = n `div` (sum (map E.dur es))
                           in map (E.scaleElt f) es

m_to_enp :: [(M.Label, M.E)] -> M.M -> E.Measure
m_to_enp assoc (M.M ts t e) =
    let f = ((denominator (M.dur e))%1)
        list = (unwrap (wrapIfNeeded (e_to_enp assoc f e)))
        list' = map wrapIfNeeded list
        list'' = adaptForTimesig ts list'
    in (E.makeMeasure ts t list'')

v_to_enp :: M.Voice -> E.Voice
v_to_enp v = let v' = M.label_voice v
                 ls = M.vleaves v'
                 assoc = zip (map M.eid ls) ls
             in fmap (m_to_enp assoc) v'
