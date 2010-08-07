module SimpleFormat (sexp2simpleFormat)
where

import Utils
import Lisp

type Time = Float

data Score = Score { scoreParts :: [Part] }
           deriving Show
data Part = Part { partVoices :: [Voice] }
           deriving Show
data Voice = Voice { voiceEvents :: [Event] }
           deriving Show
data Event = Chord Time
           | Rest Time
           deriving Show

sexp2simpleFormat :: LispVal -> Score
sexp2simpleFormat = sexp2score

sexp2score s = Score (mapcar' sexp2part s)
sexp2part s = Part (mapcar' sexp2voice s)
sexp2voice s = Voice (mapcar' sexp2event s)

sexp2event (LispInteger x) = sexp2event (LispFloat (fromInteger x))
sexp2event (LispFloat x) | x < 0 = Rest (abs x)
                         | otherwise = Chord x
sexp2event xs@(LispList _) = sexp2event (car xs)
sexp2event x = error $ "sexp2event with " ++ (show x)
