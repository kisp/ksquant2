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

module Main where

import Input
import qualified Interval as Iv
import qualified Measure as M
import Lisp
import qualified SimpleFormat as SF
import qualified SimpleFormat2 as SF2
import qualified AbstractScore as A
import qualified Enp
import qualified Lily as L
import MeasureToEnp
import MeasureToLily
import Data.List ((\\))
import Data.Maybe
import System.Environment

rationalToTime :: Rational -> Time
rationalToTime = fromRational

rationalPairToTimePair :: (Rational, Rational) -> (Time, Time)
rationalPairToTimePair (x,y) = (rationalToTime x, rationalToTime y)

type MeasureStructure = M.Voice
type Divs = [Int]

quantifyVoice :: M.Voice -> [Int] -> SF2.Events -> [M.M]
quantifyVoice ms divs v =
    let measures = A.voiceItems ms
        input = SF2.voiceChords v
        input' = Iv.ascendingIntervals input
        beats_intervals = Iv.ascendingIntervals
                          (map rationalPairToTimePair
                                   (M.measuresLeafIntervals measures))
        points = Iv.ascendingIntervals2points input'
        groups = Iv.groupPointsByIntervalls beats_intervals points
        bestDivs = (zipWith (Iv.bestDiv divs)
                     (Iv.getAscendingIntervals beats_intervals)
                     groups)
        measures' = M.measuresDivideLeafs measures (map toInteger bestDivs)
        quant_grid = M.measuresLeafIntervals measures'
        quant_grid' = Iv.ascendingIntervals (map rationalPairToTimePair quant_grid)
        quant_grid_asc = (Iv.ascendingIntervals quant_grid)
        qevents = map (Iv.quantizeIv SF2.qeventFromEvent quant_grid_asc quant_grid') input
        measures'' = M.measuresTieOrRest measures' qevents quant_grid
        getNotes (M.L dur tie label _ _) qevent =
            (M.L dur tie label (SF2.qeventNotes qevent) (SF2.qeventExpressions qevent))
        getNotes (M.R _ _) _ = error "getNotes: R"
        getNotes (M.D _  _ _) _ = error "getNotes: D"
        measures''' = M.measuresTransformLeafs getNotes measures'' qevents quant_grid
    in measures'''

buildMeasureFromLisp :: LispVal -> LispVal -> M.M
buildMeasureFromLisp (LispList [LispInteger n,LispInteger d])
                         (LispList [LispInteger tu,LispInteger t]) =
                         head (M.measuresWithBeats [(n,d)] [(tu,fromInteger t)])
buildMeasureFromLisp _ _ = error "buildMeasureFromLisp"

ensureListOfLists :: LispVal -> LispVal
ensureListOfLists (LispList []) = error "ensureListOfLists: empty list"
ensureListOfLists (LispList xs@(x:_)) | atom x = LispList [LispList xs]
                                      | otherwise = LispList xs
ensureListOfLists _ = error "ensureListOfLists"

ensureListOfIntegers :: Num b => LispVal -> [b]
ensureListOfIntegers (LispList xs) =
    map ensureInt xs
    where ensureInt (LispInteger x) = fromInteger x
          ensureInt _ = error "ensureInt"
ensureListOfIntegers _ = error "ensureListOfIntegers"

stickToLast :: [a] -> [a]
stickToLast list = list ++ repeat (last list)

measureStream :: LispVal -> LispVal -> M.Ms
measureStream ts metro = zipWith buildMeasureFromLisp (stickToLast (fromLispList ts))
                         (stickToLast (fromLispList metro))

processSimpleFormat :: LispVal -> String
processSimpleFormat (input) =
    let s = getSimple input
        sf1 = SF.sexp2simpleFormat s
        sf2 = SF2.toSimpleFormat2 sf1
        sf2end = SF2.scoreEnd sf2
        ms = M.measuresUntilTime
             (measureStream (getTimeSignatures input) (getMetronomes input))
             sf2end
        measurevoice = A.Voice ms
        divs = [1..(getMaxDiv input)] \\ getForbDivs input        
        trans = quantifyVoice measurevoice divs :: SF2.Events -> M.Ms
        mscore = fmap trans sf2 :: M.Score
        output = if isLily
                 then L.showLily (fmap vToLily mscore)
                 else printLisp (Enp.score2sexp (fmap vToEnp mscore))
    in output ++ "\n"
    where getSimple x = fromMaybe (error "Could not find :simple") (getf x (LispKeyword "SIMPLE"))
          getTimeSignatures x = case getf x (LispKeyword "TIME-SIGNATURES") of
                                  Just s -> ensureListOfLists s
                                  Nothing -> error "Could not find :time-signatures"
          getMetronomes x = case getf x (LispKeyword "METRONOMES") of
                                  Just s -> ensureListOfLists s
                                  Nothing -> error "Could not find :metronomes"
          getMaxDiv s =  case getf s (LispKeyword "MAX-DIV") of
                           Just (LispInteger x) -> fromInteger x
                           Just _ -> error "incorrect :max-div"
                           Nothing -> error "Could not find :max-div"
          getForbDivs s =  case getf s (LispKeyword "FORBIDDEN-DIVS") of
                           Just xs@(LispList _) -> ensureListOfIntegers xs
                           Just (LispSymbol "NIL") -> []
                           Just _ -> error "incorrect :forbidden-divs"
                           Nothing -> error "Could not find :forbidden-divs"
          isLily = False

processParsedInput :: [LispVal] -> String
processParsedInput [s] = processSimpleFormat s
processParsedInput [s,_] = processSimpleFormat s
processParsedInput _ = error "processParsedInput called on an unexpected number of forms"

processInput :: String -> String
processInput input = case parseLisp input of
  Right parsedInput -> processParsedInput parsedInput
  Left err -> error $ "parse error " ++ show err

main :: IO ()
main = do
  args <- getArgs
  putOutput args . processInput =<< getInput args
  where getInput [] = getContents
        getInput [i] = readFile i
        getInput [i,_] = readFile i
        getInput args = error $ "getInput with args " ++ show args ++ "?"
        putOutput [] s = putStrLn s
        putOutput [_] s = putStrLn s
        putOutput [_,o] s = writeFile o s
        putOutput args _ = error $ "putOutput with args " ++ show args ++ "?"
