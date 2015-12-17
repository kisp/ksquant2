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

import Utils
import qualified Interval as Iv
import qualified Measure as M
import Lisp
import qualified SimpleFormat as SF
import qualified SimpleFormat2 as SF2
import qualified AbstractScore as A
import qualified Enp
import qualified Lily as L
import AdjoinTies
import MeasureToEnp
import MeasureToLily
import Data.List ((\\))
import Data.Maybe
import Data.Either.Unwrap
import Control.Monad
import System.Environment

rationalToTime :: Rational -> Time
rationalToTime = fromRational

rationalPairToTimePair :: (Rational, Rational) -> (Time, Time)
rationalPairToTimePair (x,y) = (rationalToTime x, rationalToTime y)

type MeasureStructure = M.Voice
type Divs = [Int]

type Time = Float

quantifyVoice :: M.Ms -> [Divs] -> SF2.Events -> Err M.Ms
quantifyVoice measures divs v =
    let input = SF2.voiceChords v
        input' = Iv.ascendingIntervals input
        beats_intervals = Iv.ascendingIntervals
                          (map rationalPairToTimePair
                                   (M.measuresLeafIntervals measures))
        points = Iv.ascendingIntervals2points input'
        groups = Iv.groupPointsByIntervalls beats_intervals points
        bestDivs = zipWith3 Iv.bestDiv
                     divs
                     (Iv.getAscendingIntervals beats_intervals)
                     groups
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
        measures'''' = map adjoinTies measures'''
    in Right measures''''

buildMeasureFromLisp :: LispVal -> LispVal -> M.M
buildMeasureFromLisp (LispList [LispInteger n,LispInteger d])
                         (LispList [LispInteger tu,LispInteger t]) =
                         head (M.measuresWithBeats [(n,d)] [(tu,fromInteger t)])
buildMeasureFromLisp _ _ = error "buildMeasureFromLisp"

ensureListOfLists :: LispVal -> Err LispVal
ensureListOfLists (LispList []) = Left "ensureListOfLists: empty list"
ensureListOfLists (LispList xs@(x:_)) | atom x = Right $ LispList [LispList xs]
                                      | otherwise = Right $ LispList xs
ensureListOfLists _ = Left "ensureListOfLists"

ensureListOfIntegers :: Num a => LispVal -> Err [a]
ensureListOfIntegers (LispList xs) =
    mapM ensureInt xs
    where ensureInt (LispInteger x) = Right $ fromInteger x
          ensureInt _ = Left "ensureInt"
ensureListOfIntegers _ = Left "ensureListOfIntegers"

ensureList :: LispVal -> LispVal
ensureList x@(LispList (LispList _ : _)) = x
ensureList x@_                           = LispList [x]

measureStream' :: (LispVal, LispVal) -> M.Ms
measureStream' (ts, metro) = zipWith buildMeasureFromLisp
                               (stickToLast (fromLispList ts))
                               (stickToLast (fromLispList metro))

getTimeSignatures :: LispVal -> Err LispVal
getTimeSignatures x = case getf x (LispKeyword "TIME-SIGNATURES") of
  Just s -> ensureListOfLists s
  Nothing -> Left "Could not find :time-signatures"

getMetronomes :: LispVal -> Err LispVal
getMetronomes x = case getf x (LispKeyword "METRONOMES") of
  Just s -> ensureListOfLists s
  Nothing -> Left "Could not find :metronomes"

getMaxDiv :: LispVal -> Err [Int]
getMaxDiv s =  case getf s (LispKeyword "MAX-DIV") of
  Just x -> ensureListOfIntegers $ ensureList x
  Nothing -> Left "Could not find :max-div"

getForbDivs :: Num b => LispVal -> Err [b]
getForbDivs s =  case getf s (LispKeyword "FORBIDDEN-DIVS") of
  Just xs@(LispList _) -> ensureListOfIntegers xs
  Just (LispSymbol "NIL") -> Right []
  Just _ -> Left "incorrect :forbidden-divs"
  Nothing -> Left "Could not find :forbidden-divs"

measuresUntilTime :: Float -> M.Ms -> Err M.Ms
measuresUntilTime a b = Right $ M.measuresUntilTime b a

mkTrans :: LispVal -> SF2.Score -> Err (SF2.Events -> Err M.Ms)
mkTrans input sf2 = do
  let sf2end = SF2.scoreEnd sf2
  tsmetro <- liftM2 (,) (getTimeSignatures input) (getMetronomes input)
  measures <- measuresUntilTime sf2end (measureStream' tsmetro)
  maxdiv <- getMaxDiv input
  forbid <- getForbDivs input
  let divs = zipWith (\m f -> [1..m] \\ f)
             (stickToLast maxdiv)
             (stickToLast [forbid])
  return $ quantifyVoice measures divs

getSimple :: LispVal -> Err LispVal
getSimple x = fromMaybe (Left "Could not find :simple") (liftM Right (getf x (LispKeyword "SIMPLE")))

unwrapLeft :: A.Score (Err M.Ms) -> Err M.Score
unwrapLeft = Right . fmap fromRight

processSimpleFormat ::  Bool -> LispVal -> Err String
processSimpleFormat isLily input = do
  sf2 <- liftM (fmap (SF2.voiceToSimpleFormat2 . mapcar' SF.sexp2event) . fromSexp) . getSimple $ input
  trans <- mkTrans input sf2
  mscore <- liftM (fmap trans) (Right sf2)
  if isLily
    then liftM (L.showLily . fmap vToLily) $ unwrapLeft mscore
    else liftM (printSexp . fmap (Enp.voice2sexp . vToEnp)) $ unwrapLeft mscore

appendNewline :: String -> Err String
appendNewline s = Right $ s ++ "\n"

processParsedInput :: [LispVal] -> Err String
processParsedInput [s] = processSimpleFormat False s >>= appendNewline
processParsedInput [s,_] = processSimpleFormat False s >>= appendNewline
processParsedInput _ = Left "processParsedInput called on an unexpected number of forms"

processInput :: String -> Err String
processInput input = parseLisp input >>= processParsedInput

main :: IO ()
main = do
  args <- getArgs
  putOutput args . processInput =<< getInput args
  where getInput [] = getContents
        getInput [i] = readFile i
        getInput [i,_] = readFile i
        getInput args = error $ "getInput with args " ++ show args ++ "?"
        putOutput [] (Right s) = putStrLn s
        putOutput [_] (Right s) = putStrLn s
        putOutput [_,o] (Right s) = writeFile o s
        putOutput args (Right _) = error $ "putOutput with args " ++ show args ++ "?"
        putOutput _ (Left err) = error err
