module MeasureToLily (m_to_lily) where
import qualified Measure as M
import qualified Lily as L
import Data.Ratio

log2 1 = 0
log2 n | even n = 1 + (log2 (div n 2))

dur_to_lily :: M.Rat -> L.Dur
dur_to_lily d = (L.Dur (base d) (dots d))
    where dots d = case (numerator d) of
                     1 -> 0
                     3 -> 1
                     7 -> 2
                     15 -> 3
          base d = L.power_to_simple_dur ((log2 (denominator d)) - dots d)

e_to_lily :: M.E -> [L.Elt]
e_to_lily (M.L d) = [L.Note (dur_to_lily d) False]
e_to_lily (M.D d r es) | r == 1 = concatMap e_to_lily es
                       | otherwise = let n' = numerator r
                                         d' = denominator r
                                     in [L.Times n' d' (concatMap e_to_lily es)]

m_to_lily :: M.M -> L.Measure
m_to_lily (M.M (n,d) tempo e) = (L.Measure n d (e_to_lily e))
