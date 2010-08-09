module Main where

import Input
import qualified Interval as Iv
import qualified Measure as M
import Lisp
import qualified SimpleFormat as SF
import qualified SimpleFormat2 as SF2
import qualified AbstractScore as A
import qualified Enp as Enp
import MeasureToEnp

----------------

rational_to_time :: Rational -> Time
rational_to_time x = fromRational x

rational_pair_to_time_pair (x,y) = (rational_to_time x, rational_to_time y)

----------------
divs = [1..8] :: [Int]

----------------

-- groups' = Iv.groupPointsByIntervalls quant_grid' points

-- make_qevent ivs ((start_i,end_i),e) = QEvent (Iv.start (ivs!!start_i)) (Iv.start (ivs!!end_i)) [e]

-- qevents = Iv.ascending_intervals (map ((make_qevent quant_grid) . (Iv.quantize_iv quant_grid')) input)

----------------

nice_show label obj = do
  putStrLn "------------"
  putStrLn $ (label ++ ":\n" ++ (show obj))

-- main2 = do
  -- nice_show "input" input
  -- nice_show "input'" input'
  -- nice_show "groups" groups
  -- nice_show "best_divs" best_divs
  -- nice_show "quant_grid" quant_grid
  -- nice_show "groups'" groups'
  -- nice_show "qevents" qevents
  -- L.exportLily "atest" (map m_to_lily measures')

---------------------

type MeasureStructure = M.Voice

quantifyVoice :: MeasureStructure -> SF2.Voice -> M.Voice
quantifyVoice ms v =
    let measures = A.voiceItems ms
        input = A.voiceItems v :: [SF2.Event]
        input' = Iv.ascending_intervals input
        beats_intervals = Iv.ascending_intervals
                          (map rational_pair_to_time_pair
                                   (M.measures_leaf_intervals measures))
        points = Iv.ascending_intervals2points input'
        groups = Iv.groupPointsByIntervalls beats_intervals points
        best_divs = (map (uncurry (Iv.best_div divs))
                     (zip (Iv.get_ascending_intervals beats_intervals) groups))
        measures' = M.measures_divide_leafs measures (map toInteger best_divs)
        quant_grid = M.measures_leaf_intervals measures'
        quant_grid' = Iv.ascending_intervals (map rational_pair_to_time_pair quant_grid)
        quant_grid_asc = (Iv.ascending_intervals quant_grid)
        qevents = map (Iv.quantize_iv SF2.qevent_from_event quant_grid_asc quant_grid') input :: [SF2.QEvent]
        measures'' = M.measures_tie_or_rest measures' qevents quant_grid
        getNotes (M.L dur tie label _) qevent = (M.L dur tie label (SF2.qevent_notes qevent))
        getNotes (M.R _ _) _ = error "getNotes: R"
        getNotes (M.D _  _ _) _ = error "getNotes: D"
        measures''' = M.measures_transform_leafs getNotes measures'' qevents quant_grid
    in A.Voice measures'''

buildMeasureFromLisp :: LispVal -> LispVal -> M.M
buildMeasureFromLisp (LispList [LispInteger n,LispInteger d])
                         (LispList [LispInteger _,LispInteger t]) =
                         head (M.measures_with_beats [(n,d)] [fromInteger t])
buildMeasureFromLisp _ _ = error "buildMeasureFromLisp"

ensureListOfLists :: LispVal -> LispVal
ensureListOfLists (LispList []) = error "ensureListOfLists: empty list"
ensureListOfLists (LispList xs@(x:_)) | atom x = LispList [LispList xs]
                                      | otherwise = LispList xs
ensureListOfLists _ = error "ensureListOfLists"

stickToLast list = list ++ (repeat (last list))

measureStream :: LispVal -> LispVal -> [M.M]
measureStream ts metro = (map (uncurry buildMeasureFromLisp)
                                  (zip (stickToLast (fromLispList ts))
                                           (stickToLast (fromLispList metro))))

processSimpleFormat :: Lisp.LispVal -> String
processSimpleFormat input =
    let s = getSimple input     
        sf1 = SF.sexp2simpleFormat s :: SF.Score
        sf2 = SF2.toSimpleFormat2 sf1 :: SF2.Score
        sf2end = SF2.scoreEnd sf2
        ms = M.measures_until_time (measureStream (getTimeSignatures input) (getMetronomes input)) sf2end
        measurevoice = A.Voice $ ms 
        trans = (quantifyVoice measurevoice) :: SF2.Voice -> M.Voice
        mscore = (A.mapVoices trans sf2) :: M.Score
        enp = (A.mapVoices v_to_enp mscore) :: Enp.Score
    in printLisp (Enp.score2sexp enp)
    where getSimple x = case getf x (LispKeyword "SIMPLE") of
                          Just s -> s
                          Nothing -> error "Could not find :simple"
          getTimeSignatures x = case getf x (LispKeyword "TIME-SIGNATURES") of
                                  Just s -> ensureListOfLists s
                                  Nothing -> error "Could not find :time-signatures"
          getMetronomes x = case getf x (LispKeyword "METRONOMES") of
                                  Just s -> ensureListOfLists s
                                  Nothing -> error "Could not find :metronomes"
                                                                   

main = do
  s <- getContents
  case (parseLisp s) of
    Right [s] -> (putStrLn . processSimpleFormat) s
    Right [s,_] -> (putStrLn . processSimpleFormat) s
    Right _ -> error "parseLisp of stdin returned an unexpected number of forms"
    Left err -> do { print err ; error "parse error" }
