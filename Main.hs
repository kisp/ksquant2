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

{-# LANGUAGE FlexibleContexts #-}

module Main (main)
where

import Types (Time, Err, DivChoicesSeq, WInt, BestDivsSeq, QuantGrid)
import Utils (stickToLast, repeatList)
import qualified Interval as Iv (ascendingIntervals
                                , ascendingIntervals2points
                                , groupPointsByIntervalls
                                , getAscendingIntervals)
import qualified Measure as M (Ms
                              , measuresLeafIntervals
                              , measuresDivideLeafs
                              , measuresTieOrRest
                              , E(L,R,D)
                              , M
                              , measuresTransformLeafs
                              , measuresWithBeats
                              , measuresUntilTime
                              , measureNumLeaf
                              , Score)
import Lisp (LispVal(LispList, LispInteger, LispKeyword)
            , atom
            , fromLispList
            , getf
            , fromSexp
            , mapcar'
            , printSexp
            , parseLisp)
import qualified SimpleFormat as SF (Score, sexp2event)
import qualified SimpleFormat2 as SF2 (Events
                                      , qeventFromEvent
                                      , qeventNotes
                                      , qeventExpressions
                                      , Score
                                      , scoreEnd
                                      , voiceToSimpleFormat2
                                      , withoutEndMarker
                                      , QEvents)
import qualified AbstractScore as A (Score)
import qualified Enp as E (voice2sexp)
import qualified Lily as L (showLily)
import qualified Quantize as Qu (bestDiv, quantizeIv)
import AdjoinTies (adjoinTies)
import MeasureToEnp (vToEnp)
import MeasureToLily (vToLily)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.Either.Unwrap (fromRight)
import Control.Monad (liftM, liftM2, unless)
import System.IO (hPutStrLn, hPutStr, stderr)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getArgs)
import System.Console.GetOpt (OptDescr(Option)
                             , ArgDescr(ReqArg, NoArg)
                             , usageInfo
                             , getOpt
                             , ArgOrder(RequireOrder))

rationalToTime :: Rational -> Time
rationalToTime = fromRational

rationalPairToTimePair :: (Rational, Rational) -> (Time, Time)
rationalPairToTimePair (x,y) = (rationalToTime x, rationalToTime y)

computeBestDivs :: M.Ms -> DivChoicesSeq -> SF2.Events -> BestDivsSeq
computeBestDivs measures divChoicesSeq input =
  let
    input' = Iv.ascendingIntervals input
    beats_intervals = Iv.ascendingIntervals
                      (map rationalPairToTimePair
                        (M.measuresLeafIntervals measures))
    beats_intervals' = (Iv.getAscendingIntervals beats_intervals)
    points = Iv.ascendingIntervals2points input'
    groups = Iv.groupPointsByIntervalls beats_intervals points
  in zipWith3 Qu.bestDiv
     divChoicesSeq
     beats_intervals'
     groups

computeQEvents :: QuantGrid -> SF2.Events -> SF2.QEvents
computeQEvents quant_grid input =
  let
    quant_grid' = Iv.ascendingIntervals (map rationalPairToTimePair quant_grid)
    quant_grid_asc = (Iv.ascendingIntervals quant_grid)
  in
    map (Qu.quantizeIv SF2.qeventFromEvent quant_grid_asc quant_grid') input

quantifyVoice :: M.Ms -> DivChoicesSeq -> SF2.Events -> M.Ms
quantifyVoice measures divChoicesSeq voice =
  let
    getNotes (M.L dur tie label _ _) qevent =
      (M.L dur tie label (SF2.qeventNotes qevent) (SF2.qeventExpressions qevent))
    getNotes (M.R _ _) _ = error "getNotes: R"
    getNotes (M.D _  _ _) _ = error "getNotes: D"

    measuresTieOrRest' a b m = M.measuresTieOrRest m a b

    measuresTransformLeafs' a b c m = M.measuresTransformLeafs a m b c
  in
    let input = SF2.withoutEndMarker voice
        bestDivs = computeBestDivs measures divChoicesSeq input
        measures' = M.measuresDivideLeafs measures (map toInteger bestDivs)
        quant_grid = M.measuresLeafIntervals measures'
        qevents = computeQEvents quant_grid input
        transformMeasures =
          (map adjoinTies .
           measuresTransformLeafs' getNotes qevents quant_grid .
           measuresTieOrRest' qevents quant_grid)
    in transformMeasures measures'

quantifyVoiceOrErr :: M.Ms -> DivChoicesSeq -> SF2.Events -> Err M.Ms
quantifyVoiceOrErr measures divChoicesSeq voice =
  Right (quantifyVoice measures divChoicesSeq voice)

buildMeasureFromLisp :: LispVal -> LispVal -> M.M
buildMeasureFromLisp (LispList [LispInteger n,LispInteger d])
                         (LispList [LispInteger tu,LispInteger t]) =
                         head (M.measuresWithBeats [(n,d)] [(tu,fromInteger t)])
buildMeasureFromLisp _ _ = error "buildMeasureFromLisp"

ensureListOfLists :: LispVal -> Err LispVal
ensureListOfLists (LispList []) = Left "ensureListOfLists: empty list"
ensureListOfLists (LispList xs@(x:_)) | atom x = Right $ LispList [LispList xs]
                                      | otherwise = Right $ LispList xs
ensureListOfLists x = Left $ "ensureListOfLists: not a list: " ++ (show x)

ensureListOfIntegers :: Num a => LispVal -> Err [a]
ensureListOfIntegers (LispList xs) =
    mapM ensureInt xs
    where ensureInt (LispInteger x) = Right $ fromInteger x
          ensureInt v = Left $ "ensureInt: " ++ (show v)
ensureListOfIntegers x = Left $ "ensureListOfIntegers: " ++ (show x)

ensureList :: LispVal -> LispVal
ensureList x@(LispList _) = x
ensureList x              = LispList [x]

ensureList2 :: LispVal -> LispVal
ensureList2 x@(LispList (LispList _ : _)) = x
ensureList2 x@_                           = LispList [x]

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

getMaxDiv :: LispVal -> Err [WInt]
getMaxDiv s =  case getf s (LispKeyword "MAX-DIV") of
  Just x  -> ensureListOfIntegers $ ensureList x
  Nothing -> Left "Could not find :max-div"

getForbDivs :: LispVal -> Err [[WInt]]
getForbDivs s =  case getf s (LispKeyword "FORBIDDEN-DIVS") of
  Just x  -> mapM ensureListOfIntegers (fromSexp (ensureList2 x))
  Nothing -> Left "Could not find :forbidden-divs"

measuresUntilTime :: Time -> M.Ms -> Err M.Ms
measuresUntilTime a b = Right $ M.measuresUntilTime b a

getSimple :: LispVal -> Err LispVal
getSimple x = fromMaybe (Left "Could not find :simple") (liftM Right (getf x (LispKeyword "SIMPLE")))

unwrapLeft :: A.Score (Err M.Ms) -> Err M.Score
unwrapLeft = Right . fmap fromRight

scoreToLily :: M.Score -> String
scoreToLily = L.showLily . fmap vToLily

scoreToEnp :: M.Score -> String
scoreToEnp = printSexp . fmap (E.voice2sexp . vToEnp)

scoreToOutputFormat :: Options -> Err (M.Score -> String)
scoreToOutputFormat
  Options { optOutputFormat = "ly" } = Right (appendNewline . scoreToLily)
scoreToOutputFormat
  Options { optOutputFormat = "enp" } = Right (appendNewline . scoreToEnp)
scoreToOutputFormat
  Options { optOutputFormat = x } = Left $ "unknown output format: " ++ x

mkTrans :: LispVal -> SF2.Score -> Err (SF2.Score -> A.Score (Err M.Ms))
mkTrans input sf2 = do
  let sf2end = SF2.scoreEnd sf2
  tsmetro <- liftM2 (,) (getTimeSignatures input) (getMetronomes input)
  measures <- measuresUntilTime sf2end (measureStream' tsmetro)
  maxdiv <- getMaxDiv input
  forbid <- getForbDivs input
  let divs = zipWith (\m f -> [1..m] \\ f)
             (stickToLast maxdiv)
             (stickToLast forbid)
  let beatDivs = repeatList divs (map M.measureNumLeaf measures)
  let trans = quantifyVoiceOrErr measures beatDivs
  return $ fmap trans

simple2sf2_score ::  LispVal -> SF2.Score
simple2sf2_score simple =
  let
    lispVal2Score :: LispVal -> A.Score LispVal
    lispVal2Score = fromSexp
    score = lispVal2Score simple :: A.Score LispVal
    sf_score = fmap (mapcar' SF.sexp2event) score :: SF.Score
    sf2_score = fmap SF2.voiceToSimpleFormat2 sf_score :: SF2.Score
  in sf2_score

processParsedInput ::  Options -> LispVal -> Err String
processParsedInput opts input =
  do
    simple <- getSimple input :: Err LispVal
    let sf2_score = simple2sf2_score simple :: SF2.Score

    trans <- mkTrans input sf2_score :: Err (SF2.Score -> A.Score (Err M.Ms))
    let mscore = (trans sf2_score :: A.Score (Err M.Ms))

    mscore' <- unwrapLeft mscore
    scoreFormatter <- scoreToOutputFormat opts
    return $ scoreFormatter mscore'

processInput :: Options -> String -> Err String
processInput opts input = do
  forms <- parseLisp input
  let (first_form:_) = forms
  processParsedInput opts first_form

appendNewline :: String -> String
appendNewline s = s ++ "\n"

data Options = Options  { optVerbose        :: Bool
                        , optInputFormat    :: String
                        , optOutputFormat   :: String
                        }

startOptions :: Options
startOptions = Options  { optVerbose        = False
                        , optInputFormat    = "sf"
                        , optOutputFormat   = "enp"
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "r" ["read", "from"]
        (ReqArg
            (\arg opt -> return opt { optInputFormat = arg })
            "FORMAT")
        "Input format"

    , Option "w" ["write", "to"]
        (ReqArg
            (\arg opt -> return opt { optOutputFormat = arg })
            "FORMAT")
        "Output format"

    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"

    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "KSQuant2 0.01"
                exitWith ExitSuccess))
        "Print version"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr (usageInfo usageHeader options)
                exitWith ExitSuccess))
        "Show help"
    ]

usageHeader :: String
usageHeader = "Usage: ksquant2 [OPTIONS]"

handleInvalidOptions :: [String] -> IO ()
handleInvalidOptions errors = do
  hPutStrLn stderr $ concat errors
  hPutStr stderr $ usageInfo usageHeader options
  exitWith $ ExitFailure 1

main :: IO ()
main = do
  args <- getArgs

  let (actions, nonOptions, errors) = getOpt RequireOrder options args

  unless (errors == [] && (length nonOptions) <= 2) (handleInvalidOptions errors)

  opts <- foldl (>>=) (return startOptions) actions

  let (nonOptions', inputHandler) = getInputHandler nonOptions
  let outputHandler = getOutputHandler nonOptions'

  outputHandler . processInput opts =<< inputHandler

  where
    getInputHandler :: [String] -> ([String], IO String)
    getInputHandler [] = ([], getContents)
    getInputHandler ("-":r) = (r, getContents)
    getInputHandler (i:r) = (r, readFile i)

    getOutputHandler :: [String] -> Either String String -> IO ()
    getOutputHandler [] (Right s) = putStrLn s
    getOutputHandler [o] (Right s) = writeFile o s
    getOutputHandler args (Right _) = error $ "getOutputHandler with args " ++ show args ++ "?"
    getOutputHandler _ (Left err) = error err
