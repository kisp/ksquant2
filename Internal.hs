-- module Internal (measures_from_timesigs_tempos)
module Internal
where
import Input
import qualified Interval as Iv
import Data.Ratio

data E = D Rational Rational [E]
       | L Rational
         deriving Show

edur (L d) = d
edur (D d _ _) = d

l dur = L dur

d ratio xs = D dur ratio xs
    where dur = ratio * sum (map edur xs)

type TimeSig = (Int, Int)

data MeasureElt = Leaf Time Time
                | Div Time Time (Iv.AscendingIntervals MeasureElt)
                  deriving Show

instance Iv.Interval MeasureElt Time where
    start (Leaf x _) = x
    start (Div x _ _) = x
    end   (Leaf _ x) = x
    end   (Div _ x _) = x

-- we have timesig, tempo, start, end
data Measure = Measure TimeSig Time MeasureElt
               deriving Show

instance Iv.Interval Measure Time where
    start (Measure _ _ x) = Iv.start x
    end   (Measure _ _ x) = Iv.end x

beat_length :: Time -> Time
beat_length tempo = 60 / tempo

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

-- measure_from_start_tempo timesig tempo start =
--     let (numer,_) = timesig
--         dur = intToFloat numer * beat_length tempo
--     in Measure timesig tempo start (start + dur)
--            (Iv.divide_interval (start,(start + dur)) (intToFloat numer))

-- measures_from_timesigs_tempos :: [TimeSig] -> [Time] -> Iv.AscendingIntervals Measure
-- measures_from_timesigs_tempos timesigs tempos = Iv.ascending_intervals (foldl r [] (zip timesigs tempos))
--     where r :: [Measure] -> (TimeSig,Time) -> [Measure]
--           r preds (timesig,tempo) = preds ++ [measure_from_start_tempo timesig tempo (l preds)]
--           l [] = 0
--           l preds = Iv.end (last preds)
