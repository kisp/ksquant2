module SimpleFormat2 (toSimpleFormat2
                     ,Score
                     ,Part
                     ,Voice)
where

import Utils
import qualified SimpleFormat as SF1
import qualified AbstractScore as A

type Time = Float
type Start = Time
type End = Time

type Score = A.Score Event
type Part = A.Part Event
type Voice = A.Voice Event

data Event = Chord Start End
           deriving Show

toSimpleFormat2 :: SF1.Score -> Score
toSimpleFormat2 s = A.Score (map partToSimpleFormat2 (A.scoreParts s))

partToSimpleFormat2 :: SF1.Part -> Part
partToSimpleFormat2 s = A.Part (map voiceToSimpleFormat2 (A.partVoices s))

voiceToSimpleFormat2 :: SF1.Voice -> Voice
voiceToSimpleFormat2 v =
    let events = A.voiceMeasures v
        startEndPairs = (neighbours (map SF1.eventStart events))
    in A.Voice $ map trans (zip events startEndPairs)
    where trans (event,(start,end)) = Chord start end
