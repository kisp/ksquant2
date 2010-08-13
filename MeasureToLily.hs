module MeasureToLily
       (mToLily,vToLily) where
import qualified Measure as M
import qualified Lily as L
import qualified AbstractScore as A
import Data.Ratio

log2 1 = 0
log2 n | even n = 1 + log2 (div n 2)
log2 _ = error "log2"

durToLily :: Rational -> L.Dur
durToLily d = L.Dur (base d) (dots d)
    where dots d = case (numerator d) of
                     1 -> 0
                     3 -> 1
                     7 -> 2
                     15 -> 3
                     _ -> error "durToLily"
          base d = L.powerToSimpleDur (log2 (denominator d) - dots d)

eToLily :: M.E -> [L.Elt]
eToLily (M.L d tie _ _) = [L.Note (durToLily d) tie]
eToLily (M.R d _) = [L.Rest (durToLily d)]
eToLily (M.D _ r es) | r == 1 = concatMap eToLily es
                       | otherwise = let n' = fromInteger (numerator r)
                                         d' = fromInteger (denominator r)
                                     in [L.Times n' d' (concatMap eToLily es)]

mToLily :: M.M -> L.Measure
mToLily (M.M (n,d) _ e) = L.Measure (fromInteger n) (fromInteger d) (eToLily e)

vToLily :: M.Voice -> L.Voice
vToLily v = A.Voice $ map mToLily (A.voiceItems v)
