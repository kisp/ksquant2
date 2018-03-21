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

import qualified Types as T (Err
                            , DivChoicesSeq
                            , BestDivsSeq
                            , QuantGrid
                            )
import qualified Options as O (Options(..), PureMain)
import qualified Utils as U (stickToLast
                            , repeatList
                            , rationalPairToTimePair
                            , appendNewline)
import IOHandler (handleIO)
import MainUtils ( unwrapLeft
                 , getSimple
                 , measureStream'
                 , measuresUntilTime
                 , addInlineOptions
                 , scoreToLily
                 , scoreToEnp
                 , scoreToDurs)
import qualified Interval as Iv (ascendingIntervals
                                , ascendingIntervals2points
                                , groupPointsByIntervalls
                                , getAscendingIntervals)
import qualified Measure as M (Ms
                              , measuresLeafIntervals
                              , measuresDivideLeafs
                              , measuresTieOrRest
                              , E(L,R,D)
                              , measuresTransformLeafs
                              , measureNumLeaf
                              , Score)
import qualified Lisp as L (LispVal()
                           , fromSexp
                           , toSexp
                           , mapcar'
                           , parseLisp)
import DursInput (dursInputToSFScore)
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
import qualified Quantize as Qu (bestDiv, quantizeIv)
import AdjoinTies (adjoinTies)
import MeasureSiblingMerge (measureSiblingMerge)
import Data.List ((\\))

type Parser = String -> T.Err ParseResult
type Processor = ParseResult -> T.Err M.Score
type Filter = M.Score -> T.Err M.Score
type Formatter = M.Score -> T.Err String

data ParseResult = SFInput L.LispVal
                 | DursInput L.LispVal

getProcessor :: O.Options -> Processor
getProcessor opts (SFInput parseResult) = do
     simple <- getSimple parseResult :: T.Err L.LispVal
     let sf_score = simple2sf_score simple :: SF.Score
     opts' <- addInlineOptions opts parseResult
     process_sf_score opts' sf_score
getProcessor opts (DursInput parseResult) = do
     let sf_score = dursInputToSFScore parseResult
     process_sf_score opts sf_score

getParser :: O.Options -> T.Err Parser
getParser O.Options { O.optInputFormat = "sf" } = Right parseAsSFInput
getParser O.Options { O.optInputFormat = "durs" } = Right parseAsDursInput
getParser O.Options { O.optInputFormat = f } = Left $ "unknown input format " ++ f

getFilter :: O.Options -> T.Err Filter
getFilter O.Options { O.optMeasureSiblingMerge = True } = Right (Right . fmap (map measureSiblingMerge))
getFilter _ = Right Right

getFormatter :: O.Options -> T.Err Formatter
getFormatter O.Options { O.optOutputFormat = "enp" } = Right (Right . U.appendNewline . scoreToEnp)
getFormatter O.Options { O.optOutputFormat = "ly" } = Right (Right . U.appendNewline . scoreToLily)
getFormatter O.Options { O.optOutputFormat = "durs" } = Right (Right . U.appendNewline . scoreToDurs)
getFormatter O.Options { O.optOutputFormat = f } = Left $ "unknown output format " ++ f

parseAsSFInput :: Parser
parseAsSFInput input = do
  forms <- L.parseLisp input
  let (first_form:_) = forms
  return $ SFInput first_form

parseAsDursInput :: Parser
parseAsDursInput input = do
  forms <- L.parseLisp input
  return $ DursInput (L.toSexp forms)

computeBestDivs :: M.Ms -> T.DivChoicesSeq -> SF2.Events -> T.BestDivsSeq
computeBestDivs measures divChoicesSeq input =
  let
    input' = Iv.ascendingIntervals input
    beats_intervals = Iv.ascendingIntervals
                      (map U.rationalPairToTimePair
                        (M.measuresLeafIntervals measures))
    beats_intervals' = Iv.getAscendingIntervals beats_intervals
    points = Iv.ascendingIntervals2points input'
    groups = Iv.groupPointsByIntervalls beats_intervals points
  in zipWith3 Qu.bestDiv
     divChoicesSeq
     beats_intervals'
     groups

computeQEvents :: T.QuantGrid -> SF2.Events -> SF2.QEvents
computeQEvents quant_grid input =
  let
    quant_grid' = Iv.ascendingIntervals (map U.rationalPairToTimePair quant_grid)
    quant_grid_asc = Iv.ascendingIntervals quant_grid
  in
    map (Qu.quantizeIv SF2.qeventFromEvent quant_grid_asc quant_grid') input

quantifyVoice :: M.Ms -> T.DivChoicesSeq -> SF2.Events -> M.Ms
quantifyVoice measures divChoicesSeq voice =
  let
    getNotes (M.L dur tie label _ _) qevent =
      M.L dur tie label (SF2.qeventNotes qevent) (SF2.qeventExpressions qevent)
    getNotes (M.R _ _) _ = error "getNotes: R"
    getNotes M.D{} _ = error "getNotes: D"

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

quantifyVoiceOrErr :: M.Ms -> T.DivChoicesSeq -> SF2.Events -> T.Err M.Ms
quantifyVoiceOrErr measures divChoicesSeq voice =
  Right (quantifyVoice measures divChoicesSeq voice)

mkTrans :: O.Options -> SF2.Score -> T.Err (SF2.Score -> A.Score (T.Err M.Ms))
mkTrans opts sf2 = do
  let O.Options { O.optMaxDiv = maxdiv
              , O.optForbiddenDivs = forbid
              , O.optTimeSignatures = ts
              , O.optMetronomes = ms }
        = opts
  let sf2end = SF2.scoreEnd sf2
  let tsmetro = (ts, ms)
  measures <- measuresUntilTime sf2end (measureStream' tsmetro)
  let divs = zipWith (\m f -> [1..m] \\ f)
             (U.stickToLast maxdiv)
             (U.stickToLast forbid)
  let beatDivs = U.repeatList divs (map M.measureNumLeaf measures)
  let trans = quantifyVoiceOrErr measures beatDivs
  return $ fmap trans

simple2sf_score ::  L.LispVal -> SF.Score
simple2sf_score simple =
  let
    lispVal2Score :: L.LispVal -> A.Score L.LispVal
    lispVal2Score = L.fromSexp
    score = lispVal2Score simple :: A.Score L.LispVal
    sf_score = fmap (L.mapcar' SF.sexp2event) score :: SF.Score
  in sf_score

sf_score2sf2_score :: SF.Score -> SF2.Score
sf_score2sf2_score = fmap SF2.voiceToSimpleFormat2

process_sf_score ::  O.Options -> SF.Score -> T.Err M.Score
process_sf_score opts sf_score =
  do
    let sf2_score = sf_score2sf2_score sf_score :: SF2.Score

    trans <- mkTrans opts sf2_score :: T.Err (SF2.Score -> A.Score (T.Err M.Ms))
    let mscore = trans sf2_score :: A.Score (T.Err M.Ms)

    unwrapLeft mscore

processInput :: O.PureMain
processInput opts input = do
  parse <- getParser opts
  let process = getProcessor opts
  filt <- getFilter opts
  format <- getFormatter opts

  parseResult <- parse input
  processResult <- process parseResult
  filterResult <- filt processResult
  format filterResult

main :: IO ()
main = handleIO processInput
