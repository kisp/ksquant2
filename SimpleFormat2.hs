module SimpleFormat2 (toSimpleFormat2
                     ,Score
                     ,Part
                     ,Voice
                     ,Event
                     ,QEvent
                     ,sampleVoice
                     ,voiceEnd
                     ,scoreEnd
                     ,qeventFromEvent
                     ,qeventNotes)
where

import Utils
import qualified SimpleFormat as SF1
import qualified AbstractScore as A
import Interval
import qualified Lisp as L

type Time = Float
type Start = Time
type End = Time

type Score = A.Score Event
type Part = A.Part Event
type Voice = A.Voice Event

type Notes = L.LispVal

data Event = Chord Start End Notes
           deriving Show

type QStart = Rational
type QEnd = Rational

data QEvent = QChord QStart QEnd Notes
           deriving Show

qeventFromEvent :: (Interval a QStart) => a -> Event -> QEvent
qeventFromEvent quantized_iv (Chord _ _ n) =
    QChord (start quantized_iv) (end quantized_iv) n

qeventNotes (QChord _ _ notes) = notes

instance Interval Event Time where
    start (Chord s _ _) = s
    end (Chord _ e _) = e

instance Interval QEvent Rational where
    start (QChord s _ _) = s
    end (QChord _ e _) = e

toSimpleFormat2 :: SF1.Score -> Score
toSimpleFormat2 s = A.Score (map partToSimpleFormat2 (A.scoreParts s))

partToSimpleFormat2 :: SF1.Part -> Part
partToSimpleFormat2 s = A.Part (map voiceToSimpleFormat2 (A.partVoices s))

voiceToSimpleFormat2 :: SF1.Voice -> Voice
voiceToSimpleFormat2 v =
    let events = A.voiceItems v
        startEndPairs = (neighbours (map SF1.eventStart events))
    in A.Voice $ concatMap trans (zip events startEndPairs)
    where trans (SF1.Chord _ notes,(start,end)) = [Chord start end notes]
          trans _ = []

voiceEnd :: Voice -> End
voiceEnd v = (end . last) $ A.voiceItems v

scoreEnd :: Score -> End
scoreEnd s = maximum (map voiceEnd (concatMap A.partVoices (A.scoreParts s)))

sampleVoice :: Voice
sampleVoice = A.Voice []
