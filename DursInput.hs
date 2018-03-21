module DursInput (dursInputToSFScore)

where

import Types ( Time, WRat )
import Utils ( dxsToXs, oneOfEq )
import Lisp (LispVal
            , nil
            , n60
            , lispToRational
            , rationalToLisp
            , ensureList
            , car
            , cdr
            , getf'
            , readLisp'
            , fromLispList
            , toSexp)
import qualified AbstractScore as A ( singleVoice2Score )
import qualified SimpleFormat as SF ( Score, Event(..) )

tieKW :: LispVal
tieKW = readLisp' ":tie"

notesKW :: LispVal
notesKW = readLisp' ":NOTES"

removeTies :: [LispVal] -> [LispVal]
removeTies (a:t:b:r) | t == tieKW = removeTies (joinByTie a b:r)
removeTies (a:r)                  = a:removeTies r
removeTies []                     = []

joinByTie :: LispVal -> LispVal -> LispVal
joinByTie a b = toSexp [rationalToLisp newDur, notesKW, oneOfEq notesA notesB]
  where newDur = durA + durB
        durA = lispToRational (car (ensureList a))
        durB = lispToRational (car (ensureList b))
        notesA = getf' (cdr (ensureList a)) notesKW n60
        notesB = getf' (cdr (ensureList b)) notesKW n60

dursInputToSFScore :: LispVal -> SF.Score
dursInputToSFScore input = A.singleVoice2Score events
  where inputNoTies = removeTies . fromLispList $ input
        durs = map (lispToRational . car . ensureList) inputNoTies
        points = dxsToXs (map abs durs)
        listOfNotes = map durNotes inputNoTies
        events = zipWith3 createEvent
                 (durs ++ [1])
                 points
                 (listOfNotes ++ [nil])

durNotes :: LispVal -> LispVal
durNotes dur = getf' (cdr (ensureList dur)) notesKW n60

createEvent :: WRat -> Time -> LispVal -> SF.Event
createEvent dur point notes | dur < 0 = SF.Rest point
                            | dur == 0 = error "zero dur"
                            | otherwise = SF.Chord point notes nil
