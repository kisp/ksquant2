module DursInput (dursInputToSFScore)
where

import Types (Time, WRat)
import Utils (dxsToXs)
import Lisp (LispVal
            , nil
            , n60
            , lispToRational
            , mapcar'
            , ensureList
            , car
            , cdr
            , getf'
            , readLisp')
import qualified AbstractScore as A (singleVoice2Score)
import qualified SimpleFormat as SF (Score
                                    ,Event(..))

dursInputToSFScore :: LispVal -> SF.Score
dursInputToSFScore x = A.singleVoice2Score events
  where durs = mapcar' (lispToRational . car . ensureList) x
        points = dxsToXs (map abs durs)
        listOfNotes = mapcar' durNotes x
        events = zipWith3 createEvent
                 (durs ++ [1])
                 points
                 (listOfNotes ++ [nil])

durNotes :: LispVal -> LispVal
durNotes dur = getf' (cdr (ensureList dur)) (readLisp' ":NOTES") n60

createEvent :: WRat -> Time -> LispVal -> SF.Event
createEvent dur point notes | dur < 0 = SF.Rest point
                            | dur == 0 = error "zero dur"
                            | otherwise = SF.Chord point notes nil
