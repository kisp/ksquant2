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

m_to_enp :: M.M -> E.Measure
m_to_enp (M.M (n,d) tempo e) =
    case e of
      (M.D _ _ es) ->
          let e'@(M.D _ _ es') = M.wrapWithD e
          in case (e_to_enp (durs2factor (map M.dur es')) e') of
               (E.Div _ elts) ->
                   E.makeMeasure ((fromInteger n),(fromInteger d))
                        (map (E.scaleElt f) elts)
                       where f = n `div` sum (map E.dur elts)
      e -> m_to_enp (M.m (n,d) tempo (M.wrapWithD e))
      