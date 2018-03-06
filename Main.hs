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

module Main (main)
where

import Types (Time, Err, DivChoicesSeq, WInt)
import Utils
import qualified Interval as Iv
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
import Lisp
import qualified SimpleFormat as SF
import qualified SimpleFormat2 as SF2
import qualified AbstractScore as A
import qualified Enp as E (voice2sexp)
import qualified Lily as L (showLily)
import qualified Quantize as Qu (bestDiv, quantizeIv)
import AdjoinTies
import MeasureToEnp
import MeasureToLily
import Data.List ((\\))
import Data.Maybe
import Data.Either.Unwrap
import Control.Monad
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

rationalToTime :: Rational -> Time
rationalToTime = fromRational

rationalPairToTimePair :: (Rational, Rational) -> (Time, Time)
rationalPairToTimePair (x,y) = (rationalToTime x, rationalToTime y)

quantifyVoice :: M.Ms -> DivChoicesSeq -> SF2.Events -> Err M.Ms
quantifyVoice measures divChoicesSeq voice =
    let input = SF2.voiceChords voice
        input' = Iv.ascendingIntervals input
        beats_intervals = Iv.ascendingIntervals
                          (map rationalPairToTimePair
                                   (M.measuresLeafIntervals measures))
        points = Iv.ascendingIntervals2points input'
        groups = Iv.groupPointsByIntervalls beats_intervals points
        bestDivs = zipWith3 Qu.bestDiv
                     divChoicesSeq
                     (Iv.getAscendingIntervals beats_intervals)
                     groups
        measures' = M.measuresDivideLeafs measures (map toInteger bestDivs)
        quant_grid = M.measuresLeafIntervals measures'
        quant_grid' = Iv.ascendingIntervals (map rationalPairToTimePair quant_grid)
        quant_grid_asc = (Iv.ascendingIntervals quant_grid)
        qevents = map (Qu.quantizeIv SF2.qeventFromEvent quant_grid_asc quant_grid') input
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
          ensureInt v = Left $ "ensureInt: " ++ (show v)
ensureListOfIntegers _ = Left "ensureListOfIntegers"

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

mkTrans :: LispVal -> SF2.Score -> Err (SF2.Events -> Err M.Ms)
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
  return $ quantifyVoice measures beatDivs

getSimple :: LispVal -> Err LispVal
getSimple x = fromMaybe (Left "Could not find :simple") (liftM Right (getf x (LispKeyword "SIMPLE")))

unwrapLeft :: A.Score (Err M.Ms) -> Err M.Score
unwrapLeft = Right . fmap fromRight

processSimpleFormat ::  Options -> LispVal -> Err String
processSimpleFormat opts input =
  let
    Options { optOutputFormat = outputFormat } = opts
    isLily = (outputFormat == "ly")
  in do
    sf2 <- liftM (fmap (SF2.voiceToSimpleFormat2 . mapcar' SF.sexp2event) . fromSexp) . getSimple $ input
    trans <- mkTrans input sf2
    mscore <- liftM (fmap trans) (Right sf2)
    if isLily
      then liftM (L.showLily . fmap vToLily) $ unwrapLeft mscore
      else liftM (printSexp . fmap (E.voice2sexp . vToEnp)) $ unwrapLeft mscore

appendNewline :: String -> Err String
appendNewline s = Right $ s ++ "\n"

processParsedInput :: Options -> [LispVal] -> Err String
processParsedInput opts [s] = processSimpleFormat opts s >>= appendNewline
processParsedInput opts [s,_] = processSimpleFormat opts s >>= appendNewline
processParsedInput _ _ = Left "processParsedInput called on an unexpected number of forms"

processInput :: Options -> String -> Err String
processInput opts input = parseLisp input >>= processParsedInput opts

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
