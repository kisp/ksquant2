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

{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module SimpleFormat2 (voiceToSimpleFormat2
                     ,Score
                     ,Part
                     ,Voice
                     ,Event
                     ,Events
                     ,QEvent
                     ,sampleVoice
                     ,voiceEnd
                     ,scoreEnd
                     ,qeventFromEvent
                     ,qeventNotes
                     ,qeventExpressions
                     ,voiceChords)
where

import Utils
import qualified SimpleFormat as SF1
import qualified AbstractScore as A
import Interval
import qualified Lisp as L

type Time = Float
type Start = Time
type End = Time

type Score = A.Score Events
type Part = A.Part Events
type Voice = A.Voice Events

type Events = [Event]

type Notes = L.LispVal
type Expressions = L.LispVal

data Event = Chord Start End Notes Expressions
           | EndMarker Start
           deriving Show

type QStart = Rational
type QEnd = Rational

data QEvent = QChord QStart QEnd Notes Expressions
           deriving Show

qeventFromEvent :: (Interval a QStart) => a -> Event -> QEvent
qeventFromEvent quantized_iv (Chord _ _ n e) =
    QChord (start quantized_iv) (end quantized_iv) n e
qeventFromEvent _ (EndMarker _) = error "qeventFromEvent _ (EndMarker _)"

qeventNotes :: QEvent -> Notes
qeventNotes (QChord _ _ notes _) = notes
qeventExpressions :: QEvent -> Expressions
qeventExpressions (QChord _ _ _ expressions) = expressions

instance Interval Event Time where
    start (Chord s _ _ _) = s
    start (EndMarker s) = s
    end (Chord _ e _ _) = e
    end (EndMarker s) = s

instance Interval QEvent Rational where
    start (QChord s _ _ _) = s
    end (QChord _ e _ _) = e

voiceToSimpleFormat2 :: SF1.Events -> Events
voiceToSimpleFormat2 v =
    let events = v
        startEndPairs = (neighbours (map SF1.eventStart events))
        trans (SF1.Chord _ notes expressions,(start,end)) = [Chord start end notes expressions]
        trans _ = []
    in (concatMap trans (zip events startEndPairs) ++
        [EndMarker $ SF1.eventStart (last events)])

voiceEnd :: Voice -> End
voiceEnd v = (start . last) $ A.voiceItems v


voiceChords :: [a] -> [a]
voiceChords = init

scoreEnd :: Score -> End
scoreEnd s = maximum (map voiceEnd (concatMap A.partVoices (A.scoreParts s)))

sampleVoice :: Voice
sampleVoice = A.Voice []
