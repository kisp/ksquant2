module Enp (score2sexp
           ,Score(..)
           ,Part(..)
           ,Voice(..)
           ,Measure(..)
           ,Elt(..)
           ,makeMeasure
           ,scaleElt
           ,dur)
where

import Utils
import Lisp
import qualified AbstractScore as A

type Dur = Integer
type Timesig = (Integer,Integer)

type Score = A.Score Measure
type Part = A.Part Measure
type Voice = A.Voice Measure

data Measure = Measure Timesig [Elt]
           deriving Show

data Elt = Chord Dur
           | Rest Dur
           | Div Dur [Elt]
           deriving Show

dur (Chord d) = d
dur (Rest d) = d
dur (Div d _) = d

scaleElt :: Integer -> Elt -> Elt
scaleElt n (Chord d) = (Chord (n * d))
scaleElt n (Rest d) = (Rest (n * d))
scaleElt n (Div d es ) = (Div (n * d) es)

makeMeasure :: Timesig -> [Elt] -> Measure
makeMeasure (n,d) es =
    if not(check (n,d) es) then
        error $ "Enp.makeMeasure " ++ show (n,d) ++ " " ++ show es
    else Measure (n,d) es
    where check (n,d) es = n == sum (map dur es)

score2sexp :: Score -> LispVal
score2sexp e = LispList $ map part2sexp (A.scoreParts e)

part2sexp :: Part -> LispVal
part2sexp e = LispList $ map voice2sexp (A.partVoices e)

voice2sexp :: Voice -> LispVal
voice2sexp e = 
    LispList $ (map measure2sexp (A.voiceMeasures e)) ++
             fromLispList (parseLisp' ":instrument NIL :staff :treble-staff")

measure2sexp :: Measure -> LispVal
measure2sexp (Measure (n,d) xs) =
    LispList $ (map elt2sexp xs) ++
                 [LispKeyword "TIME-SIGNATURE",
                  LispList [LispInteger n,LispInteger d]]

elt2sexp :: Elt -> LispVal
elt2sexp (Chord d) = LispInteger d `cons` (parseLisp' ":notes (60)")
elt2sexp (Rest d) = LispInteger (-d) `cons` (parseLisp' ":notes (60)")
elt2sexp (Div d xs) = (LispInteger d) `cons` (LispList [(LispList (map elt2sexp xs))])
