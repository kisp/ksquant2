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

import Types (Err
             , DivChoicesSeq
             , BestDivsSeq
             , QuantGrid
             , Options(..)
             , PureMain
             )
import Utils (stickToLast
             , repeatList
             , rationalPairToTimePair
             , appendNewline
             )
import IOHandler (handleIO)
import MainUtils ( unwrapLeft
                 , getSimple
                 , measureStream'
                 , measuresUntilTime
                 , addInlineOptions
                 , scoreToLily
                 , scoreToEnp)
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
import Lisp (LispVal()
            , fromSexp
            , mapcar'
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
import qualified Quantize as Qu (bestDiv, quantizeIv)
import AdjoinTies (adjoinTies)
import Data.List ((\\))

type Parser = String -> Err ParseResult
type Processor = ParseResult -> Err M.Score
type Filter = M.Score -> Err M.Score
type Formatter = M.Score -> Err String

data ParseResult = SFInput LispVal
                 | DursInput LispVal

getProcessor :: Options -> Processor
getProcessor opts (SFInput parseResult) = do
     simple <- getSimple parseResult :: Err LispVal
     let sf_score = simple2sf_score simple :: SF.Score
     opts' <- addInlineOptions opts parseResult
     process_sf_score opts' sf_score
getProcessor opts (DursInput parseResult) = do
     let simple = parseResult
     let sf_score = simple2sf_score simple
     process_sf_score opts sf_score

getParser :: Options -> Err Parser
getParser Options { optInputFormat = "sf" } = Right parseAsSFInput
getParser Options { optInputFormat = "durs" } = Right parseAsDursInput
getParser Options { optInputFormat = f } = Left $ "unknown input format " ++ f

getFilter :: Options -> Err Filter
getFilter _ = Right (Right . id)

getFormatter :: Options -> Err Formatter
getFormatter Options { optOutputFormat = "enp" } = Right (Right . appendNewline . scoreToEnp)
getFormatter Options { optOutputFormat = "ly" } = Right (Right . appendNewline . scoreToLily)
getFormatter Options { optOutputFormat = f } = Left $ "unknown output format " ++ f

parseAsSFInput :: Parser
parseAsSFInput input = do
  forms <- parseLisp input
  let (first_form:_) = forms
  return $ SFInput first_form

parseAsDursInput :: Parser
parseAsDursInput input = do
  forms <- parseLisp input
  let (first_form:_) = forms
  return $ DursInput first_form

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

mkTrans :: Options -> SF2.Score -> Err (SF2.Score -> A.Score (Err M.Ms))
mkTrans opts sf2 = do
  let Options { optMaxDiv = maxdiv
              , optForbiddenDivs = forbid
              , optTimeSignatures = ts
              , optMetronomes = ms }
        = opts
  let sf2end = SF2.scoreEnd sf2
  let tsmetro = (ts, ms)
  measures <- measuresUntilTime sf2end (measureStream' tsmetro)
  let divs = zipWith (\m f -> [1..m] \\ f)
             (stickToLast maxdiv)
             (stickToLast forbid)
  let beatDivs = repeatList divs (map M.measureNumLeaf measures)
  let trans = quantifyVoiceOrErr measures beatDivs
  return $ fmap trans

simple2sf_score ::  LispVal -> SF.Score
simple2sf_score simple =
  let
    lispVal2Score :: LispVal -> A.Score LispVal
    lispVal2Score = fromSexp
    score = lispVal2Score simple :: A.Score LispVal
    sf_score = fmap (mapcar' SF.sexp2event) score :: SF.Score
  in sf_score

sf_score2sf2_score :: SF.Score -> SF2.Score
sf_score2sf2_score = fmap SF2.voiceToSimpleFormat2

process_sf_score ::  Options -> SF.Score -> Err M.Score
process_sf_score opts sf_score =
  do
    let sf2_score = sf_score2sf2_score sf_score :: SF2.Score

    trans <- mkTrans opts sf2_score :: Err (SF2.Score -> A.Score (Err M.Ms))
    let mscore = (trans sf2_score :: A.Score (Err M.Ms))

    unwrapLeft mscore

processInput :: PureMain
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
