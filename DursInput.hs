module DursInput (dursInputToSFScore)
where

import Types (Time, WRat)
import Utils (dxsToXs)
import Lisp (LispVal, nil, n60, lispToRational, mapcar')
import qualified AbstractScore as A (singleVoice2Score)
import qualified SimpleFormat as SF (Score
                                    ,Event(..))

dursInputToSFScore :: LispVal -> SF.Score
dursInputToSFScore x = A.singleVoice2Score events
  where durs = mapcar' lispToRational x
        points = dxsToXs (map abs durs)
        events = zipWith createEvent
                 (durs ++ [1])
                 points

createEvent :: WRat -> Time -> SF.Event
createEvent dur point | dur < 0 = SF.Rest point
                      | dur == 0 = error "zero dur"
                      | otherwise = SF.Chord point n60 nil
