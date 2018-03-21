-- This file is part of KSQuant2.

-- Copyright (c) 2010 - 2011, Kilian Sprotte. All rights reserved.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Enp (voice2sexp
           ,Score
           ,Part
           ,Voice
           ,Measure(..)
           ,Measures
           ,Elt(..)
           ,makeMeasure
           ,scaleElt
           ,dur
           ,EnpDur)

where

import Types (WInt, Tempo, Timesig)
import Data.Either.Unwrap (fromRight)
import Lisp ( LispVal(LispList, LispKeyword, LispInteger, LispFloat), clNull, parseLisp, cons )
import qualified AbstractScore as A ( Score, Part, Voice )

type EnpDur = WInt

data Measure = Measure Timesig Tempo [Elt]
           deriving (Show, Eq)

type Score = A.Score Measures
type Part = A.Part Measures
type Voice = A.Voice Measures

type Measures = [Measure]

type Tied = Bool
type Notes = LispVal
type Expressions = LispVal

-- |Tied here has ENP semantics, which is a tie "going to the left".
data Elt = Chord EnpDur Tied Notes Expressions
           | Rest EnpDur
           | Div EnpDur [Elt]
           deriving (Show, Eq)

dur :: Elt -> EnpDur
dur (Chord d _ _ _) = d
dur (Rest d) = d
dur (Div d _) = d

scaleElt :: Integer -> Elt -> Elt
scaleElt n (Chord d t notes expressions) = Chord (n * d) t notes expressions
scaleElt n (Rest d) = Rest (n * d)
scaleElt n (Div d es) = Div (n * d) es

makeMeasure :: Timesig -> Tempo -> [Elt] -> Measure
makeMeasure (n,d) t es =
    if not check then
        error $ "Enp.makeMeasure " ++ show (n,d) ++ " " ++ show es
    else Measure (n,d) t es
    where check = n == sum (map dur es)

voice2sexp :: Measures -> LispVal
voice2sexp e = LispList $ map measure2sexp e

measure2sexp :: Measure -> LispVal
measure2sexp (Measure (n,d) (tu,t) xs) =
    LispList $ map elt2sexp xs ++
                 [LispKeyword "TIME-SIGNATURE",
                  LispList [LispInteger n,LispInteger d],
                  LispKeyword "METRONOME",
                  LispList [LispInteger tu,LispInteger $ round t]]

elt2sexp :: Elt -> LispVal
elt2sexp (Chord d False notes expressions) =
    LispInteger d `cons` LispList ([LispKeyword "NOTES", notes] ++ expressionsOrEmpty expressions)
elt2sexp (Chord d True  notes expressions) =
    LispFloat (fromInteger d) `cons` LispList ([LispKeyword "NOTES", notes] ++ expressionsOrEmpty expressions)
elt2sexp (Rest d) = LispInteger (-d) `cons` LispList ((fromRight . parseLisp) ":notes (60)")
elt2sexp (Div d xs) = LispInteger d `cons` LispList [LispList (map elt2sexp xs)]

expressionsOrEmpty :: LispVal -> [LispVal]
expressionsOrEmpty expressions = if clNull expressions then []
                                 else [LispKeyword "EXPRESSIONS", expressions]
