module Lily where
import Data.List
import System.IO
import System.Cmd
import System.Directory
import System.FilePath
import Data.Ratio

data Dur = D1 | D2 | D4 | D8 | D16 | D32 | D64
	   deriving Show

data Elt = Note Dur Bool
	 | Rest Dur
	 | Times Int Int [Elt]
	   deriving Show

data Measure = Measure Int Int [Elt]
	       deriving Show

durToRatio x =
    case x of
      D1 -> 1 % 1
      D2 -> 1 % 2
      D4 -> 1 % 4
      D8 -> 1 % 8
      D16 -> 1 % 16
      D32 -> 1 % 32
      D64 -> 1 % 64

eltToRatio (Note d _) = durToRatio d
eltToRatio (Rest d) = durToRatio d
eltToRatio (Times n d xs) = (n % d) * foldr (+) 0 (map eltToRatio xs)

isCorrectmeasurelength (Measure n d xs) = (n % d) == foldr (+) 0 (map eltToRatio xs)

durToLily x = (show . denominator . durToRatio) x

eltToLily (Note d tie) = "c'" ++ (durToLily d) ++
			 if tie then "~" else ""
eltToLily (Rest d) = "r" ++ (durToLily d)
eltToLily (Times n d xs) = "\\times " ++ show n ++ "/" ++ show d ++ " { " ++
			   intercalate " " (map eltToLily xs) ++ " }"

measureToLily ((Measure n d xs), change) =
    (if change then
	 "\\time " ++ show n ++ "/" ++ show d ++ " "
     else "")
    ++
    intercalate " " (map eltToLily xs) ++ " |"

-- return a list of equal length as xs indicating if the corresponing
-- elt of xs is different from its predecessor
indicateChanges xs = True : (map (not . (uncurry (==))) (zip (drop 1 xs) xs))

measureTimeSignature (Measure n d _) = (n,d)

measureChanges xs = indicateChanges (map measureTimeSignature xs)

measuresToLily xs = intercalate "\n      " (map measureToLily (zip xs (measureChanges xs)))

lilyString xs =
  "\\version \"2.12.3\"\n" ++
  "  \\header { }\n" ++
  "  \\score {\n" ++
  "    {\n      " ++
  measuresToLily xs ++ "\n" ++
  "    }\n" ++
  "    \\layout { }\n" ++
  "  }\n"

runLily path =
  let dir = takeDirectory path
  in do
    pwd <- getCurrentDirectory
    setCurrentDirectory dir
    rawSystem "lilypond" [takeFileName path]
    setCurrentDirectory pwd

openPdf path = rawSystem "gnome-open" [replaceExtension path "pdf"]

validateMeasures xs = and (map isCorrectmeasurelength xs)

exportLily name xs =
  if validateMeasures xs then
      let path = "/tmp" </> (replaceExtension name "ly") in
      do
	outh <- openFile path WriteMode
	hPutStr outh (lilyString xs)
	hClose outh
	runLily path
	openPdf path
  else
      error "measures are not valid"

m1 = [Measure 4 4 [Note D4 False, Rest D4, Note D4 False, Note D4 False],
      Measure 4 4 [Rest D1],
      Measure 3 4 [Note D4 False, Note D4 True, Times 2 3 [Note D8 False, Note D8 False, Note D8 True]],
      Measure 3 4 [Note D2 False, Note D4 False]]
