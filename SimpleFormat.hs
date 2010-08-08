module SimpleFormat (sexp2simpleFormat
                    ,Score
                    ,Part
                    ,Voice
                    ,Event(..)
                    ,eventStart)
where

import Utils
import Lisp
import Interval

import qualified AbstractScore as A

type Time = Float

type Score = A.Score Event
type Part = A.Part Event
type Voice = A.Voice Event

data Event = Chord Time
           | Rest Time
           deriving Show

-- TODO can we use point from Interval?
eventStart (Chord x) = x
eventStart (Rest x) = x

sexp2simpleFormat :: LispVal -> Score
sexp2simpleFormat = sexp2score

sexp2score s = A.Score (mapcar' sexp2part s)
sexp2part s = A.Part (mapcar' sexp2voice s)
sexp2voice s = A.Voice (mapcar' sexp2event s)

sexp2event (LispInteger x) = sexp2event (LispFloat (fromInteger x))
sexp2event (LispFloat x) | x < 0 = Rest (abs x)
                         | otherwise = Chord x
sexp2event xs@(LispList _) = sexp2event (car xs)
sexp2event x = error $ "sexp2event with " ++ (show x)
