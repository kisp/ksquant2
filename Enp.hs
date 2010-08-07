module Enp (score2sexp
           ,Score(..)
           ,Part(..)
           ,Voice(..)
           ,Measure(..)
           ,Elt(..))
where

import Utils
import Lisp

type Dur = Integer
type Timesig = (Integer,Integer)

data Score = Score { scoreParts :: [Part] }
           deriving Show
data Part = Part { partVoices :: [Voice] }
           deriving Show
data Voice = Voice { voiceMeasures :: [Measure] }
           deriving Show
data Measure = Measure Timesig [Elt]
           deriving Show
data Elt = Chord Dur
           | Rest Dur
           | Div Dur [Elt]
           deriving Show

score2sexp :: Score -> LispVal
score2sexp e = LispList $ map part2sexp (scoreParts e)

part2sexp :: Part -> LispVal
part2sexp e = LispList $ map voice2sexp (partVoices e)

voice2sexp :: Voice -> LispVal
voice2sexp e = 
    LispList $ (map measure2sexp (voiceMeasures e)) ++
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
