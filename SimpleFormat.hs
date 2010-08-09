module SimpleFormat (sexp2simpleFormat
                    ,Score
                    ,Part
                    ,Voice
                    ,Event(..)
                    ,eventStart)
where

import Lisp

import qualified AbstractScore as A

type Time = Float
type Notes = LispVal

type Score = A.Score Event
type Part = A.Part Event
type Voice = A.Voice Event

data Event = Chord Time Notes
           | Rest Time
           deriving Show

-- TODO can we use point from Interval?
eventStart (Chord x _) = x
eventStart (Rest x) = x

sexp2simpleFormat :: LispVal -> Score
sexp2simpleFormat = sexp2score

sexp2score s = A.Score (mapcar' sexp2part s)
sexp2part s = A.Part (mapcar' sexp2voice s)
sexp2voice s = A.Voice (mapcar' sexp2event s)

n60 = readLisp "(60)"

sexp2event (LispInteger x) = sexp2event (LispFloat (fromInteger x))
sexp2event (LispFloat x) | x < 0 = Rest (abs x)
                         | otherwise = Chord x n60
sexp2event xs@(LispList _)
    = if foundAndT $ getf (cdr xs) (readLisp ":restp")
      then sexp2event (minus (car xs))
      else case sexp2event (car xs) of
             Rest d -> Rest d
             Chord d notes -> case getf (cdr xs) (readLisp ":notes") of
                                Nothing -> Chord d notes
                                Just notes' -> Chord d notes'
    where foundAndT Nothing = False
          foundAndT (Just (LispSymbol "NIL")) = False
          foundAndT (Just _) = True
sexp2event x = error $ "sexp2event with " ++ (show x)
