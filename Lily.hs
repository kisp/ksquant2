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

module Lily (showLily
            ,Score
            ,Part
            ,Voice
            ,Dur(..)
            ,Elt(..)
            ,Measure(..)
            ,Measures
            ,Name(..)
            ,Accidental(..)
            ,Pitch(..)
            ,powerToSimpleDur
            )
where

import Types (WInt)
import Data.List
import Data.Ratio
import qualified AbstractScore as A

type Score = A.Score Measures
type Part = A.Part Measures
type Voice = A.Voice Measures

type Measures = [Measure]

-- | This represents durations without dot.
data SimpleDur = D1 | D2 | D4 | D8 | D16 | D32 | D64 | D128
           deriving (Show,Enum)

-- | 1 / 2^n
powerToSimpleDur :: Int -> SimpleDur
powerToSimpleDur = toEnum

data Dur = Dur SimpleDur Int
           deriving Show

type Tied = Bool

type Octave = WInt

data Name = A | B | C | D | E | F | G
          deriving Show

data Accidental = Natural | Sharp | Flat
                deriving Show

data Pitch = Pitch Name Accidental Octave
           deriving Show

data Elt = Chord Dur [Pitch] Tied
         | Rest Dur
         | Times Int Int [Elt]
           deriving Show

data Measure = Measure Int Int [Elt]
               deriving Show

simpleDurToRatio :: SimpleDur -> Ratio Int
simpleDurToRatio x =
    case x of
      D1 -> 1 % 1
      D2 -> 1 % 2
      D4 -> 1 % 4
      D8 -> 1 % 8
      D16 -> 1 % 16
      D32 -> 1 % 32
      D64 -> 1 % 64
      D128 -> 1 % 128

pitchToLily :: Pitch -> String
pitchToLily (Pitch B Natural 3) = "b"
pitchToLily (Pitch C Natural 4) = "c'"
pitchToLily (Pitch C Sharp 4) = "cis'"
pitchToLily (Pitch D Natural 4) = "d'"
pitchToLily (Pitch F Natural 4) = "f'"
pitchToLily p = error $ "pitchToLily not implemented: " ++ show p

durToLily :: Dur -> String
durToLily (Dur x d) = (show . denominator . simpleDurToRatio) x ++ replicate d '.'

eltToLily :: Elt -> String
eltToLily (Chord d ps tie) = "<" ++ ps' ++ ">" ++ durToLily d ++ if tie then "~" else ""
  where ps' = unwords (map pitchToLily ps)
eltToLily (Rest d) = 'r' : durToLily d
eltToLily (Times n d xs) = "\\times " ++ show n ++ "/" ++ show d ++ " { " ++
                           unwords (map eltToLily xs) ++ " }"

measureToLily :: (Measure, Bool) -> String
measureToLily (Measure n d xs, change) =
    (if change then
         "\\time " ++ show n ++ "/" ++ show d ++ " "
     else "")
    ++
    unwords (map eltToLily xs) ++ " |"

-- | Return a list of equal length as xs indicating if the corresponing
--   elt of xs is different from its predecessor.
indicateChanges :: Eq b => [b] -> [Bool]
indicateChanges xs = True : map (not . uncurry (==)) (zip (drop 1 xs) xs)

measureTimeSignature :: Measure -> (Int, Int)
measureTimeSignature (Measure n d _) = (n,d)

measureChanges :: Measures -> [Bool]
measureChanges xs = indicateChanges (map measureTimeSignature xs)

measuresToLily :: Measures -> String
measuresToLily xs = intercalate "\n      " $ zipWith (curry measureToLily) xs (measureChanges xs)


voiceToLily :: Voice -> String
voiceToLily v = "{ " ++ measuresToLily (A.voiceItems v) ++ " }"

wrap :: String -> String
wrap content =
  "\\version \"2.12.3\"\n" ++
  "\\header { }\n" ++
  "\\score {\n" ++
  "  <<\n      " ++
  content ++ "\n" ++
  "  >>\n" ++
  "  \\layout { }\n" ++
  "}\n"

showLily :: Score -> String
showLily s = wrap (intercalate "\n" (map voiceToLily (concatMap A.partVoices (A.scoreParts s))))
