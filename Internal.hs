module Internal (measures_from_timesigs_tempos)
where
import Input
import qualified Interval as I

type TimeSig = (Int, Int)

-- we have timesig, tempo, start, end
data Measure = Measure TimeSig Time Time Time
               deriving Show

instance I.Interval Measure Time where
    start (Measure _ _ x _) = x
    end (Measure _ _ _ x) = x

type Measures = [Measure]

beat_length :: Time -> Time
beat_length tempo = 60 / tempo

foo = [measure_from_start_tempo (4,4) 60 0 , Measure (4,4) 72 2 3]

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

measure_from_start_tempo timesig tempo start =
    let (numer,_) = timesig
        dur = intToFloat numer * beat_length tempo
    in Measure timesig tempo start (start + dur)

measures_from_timesigs_tempos :: [TimeSig] -> [Time] -> I.AscendingIntervals Measure
measures_from_timesigs_tempos timesigs tempos = I.ascending_intervals (foldl r [] (zip timesigs tempos))
    where r :: [Measure] -> (TimeSig,Time) -> [Measure]
          r preds (timesig,tempo) = preds ++ [measure_from_start_tempo timesig tempo (l preds)]
          l [] = 0
          l preds = I.end (last preds)
