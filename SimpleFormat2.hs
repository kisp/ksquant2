module SimpleFormat2 (toSimpleFormat2
                     ,Score
                     ,Part
                     ,Voice
                     ,Event
                     ,sampleVoice)
where

import Utils
import qualified SimpleFormat as SF1
import qualified AbstractScore as A
import Interval

type Time = Float
type Start = Time
type End = Time

type Score = A.Score Event
type Part = A.Part Event
type Voice = A.Voice Event

data Event = Chord Start End
           deriving Show

instance Interval Event Time where
    start (Chord s _) = s
    end (Chord _ e) = e

toSimpleFormat2 :: SF1.Score -> Score
toSimpleFormat2 s = A.Score (map partToSimpleFormat2 (A.scoreParts s))

partToSimpleFormat2 :: SF1.Part -> Part
partToSimpleFormat2 s = A.Part (map voiceToSimpleFormat2 (A.partVoices s))

voiceToSimpleFormat2 :: SF1.Voice -> Voice
voiceToSimpleFormat2 v =
    let events = A.voiceItems v
        startEndPairs = (neighbours (map SF1.eventStart events))
    in A.Voice $ concatMap trans (zip events startEndPairs)
    where trans ((SF1.Chord _),(start,end)) = [Chord start end]
          trans _ = []

sampleVoice :: Voice
sampleVoice = A.Voice []
