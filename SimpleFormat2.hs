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
                     , Score
                     , Part
                     , Voice
                     , Event
                     , Events
                     , QEvent
                     , QEvents
                     , voiceEnd
                     , scoreEnd
                     , qeventFromEvent
                     , qeventNotes
                     , qeventExpressions
                     , withoutEndMarker)
where

import qualified Types as T (Time, WRat)
import qualified Utils as U (neighbours)
import qualified SimpleFormat as SF1 (Events, eventStart, Event(Chord))
import qualified AbstractScore as A (Score, Part, Voice, scoreParts, partVoices, voiceItems)
import qualified Interval as Iv (Interval, start, end)
import qualified Lisp as L (LispVal)

type Start = T.Time
type End = T.Time

type Score = A.Score Events
type Part = A.Part Events
type Voice = A.Voice Events

type Events = [Event]

type Notes = L.LispVal
type Expressions = L.LispVal

data Event = Chord Start End Notes Expressions
           | EndMarker Start
           deriving Show

type QStart = T.WRat
type QEnd = T.WRat

data QEvent = QChord QStart QEnd Notes Expressions
           deriving Show

type QEvents = [QEvent]

qeventFromEvent :: (Iv.Interval a QStart) => a -> Event -> QEvent
qeventFromEvent quantized_iv (Chord _ _ n e) =
    QChord (Iv.start quantized_iv) (Iv.end quantized_iv) n e
qeventFromEvent _ (EndMarker _) = error "qeventFromEvent _ (EndMarker _)"

qeventNotes :: QEvent -> Notes
qeventNotes (QChord _ _ notes _) = notes
qeventExpressions :: QEvent -> Expressions
qeventExpressions (QChord _ _ _ expressions) = expressions

instance Iv.Interval Event T.Time where
    start (Chord s _ _ _) = s
    start (EndMarker s) = s
    end (Chord _ e _ _) = e
    end (EndMarker s) = s

instance Iv.Interval QEvent Rational where
    start (QChord s _ _ _) = s
    end (QChord _ e _ _) = e

voiceToSimpleFormat2 :: SF1.Events -> Events
voiceToSimpleFormat2 v =
    let events = v
        startEndPairs = U.neighbours (map SF1.eventStart events)
        trans (SF1.Chord _ notes expressions,(start,end)) = [Chord start end notes expressions]
        trans _ = []
    in (concatMap trans (zip events startEndPairs) ++
        [EndMarker $ SF1.eventStart (last events)])

voiceEnd :: Voice -> End
voiceEnd v = (Iv.start . last) $ A.voiceItems v

withoutEndMarker :: [a] -> [a]
withoutEndMarker = init -- butlast

scoreEnd :: Score -> End
scoreEnd s = maximum (map voiceEnd (concatMap A.partVoices (A.scoreParts s)))
