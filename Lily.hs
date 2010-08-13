module Lily (showLily
            ,Score
            ,Part
            ,Voice
            ,Dur(..)
            ,Elt(..)
            ,Measure(..)
            ,powerToSimpleDur
            )
where
import Data.List
import Data.Ratio
import qualified AbstractScore as A

type Score = A.Score Measure
type Part = A.Part Measure
type Voice = A.Voice Measure

-- | This represents durations without dot.
data SimpleDur = D1 | D2 | D4 | D8 | D16 | D32 | D64 | D128
           deriving (Show,Enum)

-- | 1 / 2^n
powerToSimpleDur :: Int -> SimpleDur
powerToSimpleDur = toEnum

data Dur = Dur SimpleDur Int
           deriving Show

data Elt = Note Dur Bool
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

durToLily (Dur x d) = (show . denominator . simpleDurToRatio) x ++ replicate d '.'

eltToLily (Note d tie) = "c'" ++ durToLily d ++ if tie then "~" else ""
eltToLily (Rest d) = 'r' : durToLily d
eltToLily (Times n d xs) = "\\times " ++ show n ++ "/" ++ show d ++ " { " ++
                           intercalate " " (map eltToLily xs) ++ " }"

measureToLily (Measure n d xs, change) =
    (if change then
         "\\time " ++ show n ++ "/" ++ show d ++ " "
     else "")
    ++
    intercalate " " (map eltToLily xs) ++ " |"

-- | Return a list of equal length as xs indicating if the corresponing
--   elt of xs is different from its predecessor.
indicateChanges xs = True : map (not . uncurry (==)) (zip (drop 1 xs) xs)

measureTimeSignature (Measure n d _) = (n,d)

measureChanges xs = indicateChanges (map measureTimeSignature xs)

measuresToLily xs = intercalate "\n      " (map measureToLily (zip xs (measureChanges xs)))

voiceToLily v = "{ " ++ measuresToLily (A.voiceItems v) ++ " }"

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
