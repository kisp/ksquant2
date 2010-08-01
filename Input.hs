module Input where
import qualified Interval as Iv

type Time = Float

data Event = Event Time Time
             deriving Show

instance Iv.Interval Event Time where
    start (Event start end) = start
    end (Event start end) = end

instance Iv.Point Time Time where
    point x = x

type Events = [Event]
