module DursInput (dursInputToSFScore)

where

import qualified Types as T ( Time, WRat )
import qualified Utils as U ( dxsToXs, oneOfEq )
import qualified Lisp as L (LispVal
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
                           , toSexp
                           , fromSexp)
import qualified AbstractScore as A ( singleVoices2Score )
import qualified SimpleFormat as SF ( Score, Event(..) )

tieKW :: L.LispVal
tieKW = L.readLisp' ":tie"

notesKW :: L.LispVal
notesKW = L.readLisp' ":NOTES"

removeTies :: [L.LispVal] -> [L.LispVal]
removeTies (a:t:b:r) | t == tieKW = removeTies (joinByTie a b:r)
removeTies (a:r)                  = a:removeTies r
removeTies []                     = []

joinByTie :: L.LispVal -> L.LispVal -> L.LispVal
joinByTie a b = L.toSexp [L.rationalToLisp newDur, notesKW, U.oneOfEq notesA notesB]
  where newDur = durA + durB
        durA = L.lispToRational (L.car (L.ensureList a))
        durB = L.lispToRational (L.car (L.ensureList b))
        notesA = L.getf' (L.cdr (L.ensureList a)) notesKW L.n60
        notesB = L.getf' (L.cdr (L.ensureList b)) notesKW L.n60

dursInputToSFScore :: L.LispVal -> SF.Score
dursInputToSFScore input = A.singleVoices2Score $ map dursInputToEvents (L.fromSexp input)

dursInputToEvents :: L.LispVal -> [SF.Event]
dursInputToEvents input = events
  where inputNoTies = removeTies . L.fromLispList $ input
        durs = map (L.lispToRational . L.car . L.ensureList) inputNoTies
        points = U.dxsToXs (map abs durs)
        listOfNotes = map durNotes inputNoTies
        events = zipWith3 createEvent
                 (durs ++ [1])
                 points
                 (listOfNotes ++ [L.nil])

durNotes :: L.LispVal -> L.LispVal
durNotes dur = L.getf' (L.cdr (L.ensureList dur)) notesKW L.n60

createEvent :: T.WRat -> T.Time -> L.LispVal -> SF.Event
createEvent dur point notes | dur < 0 = SF.Rest point
                            | dur == 0 = error "zero dur"
                            | otherwise = SF.Chord point notes L.nil
