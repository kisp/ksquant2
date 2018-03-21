module MainUtils (unwrapLeft
                 , getSimple
                 , measureStream'
                 , measuresUntilTime
                 , addInlineOptions
                 , scoreToLily
                 , scoreToEnp)

where

import qualified Types as T (Err, WInt, Time)
import qualified Options as O (Options(..))
import qualified Utils as U (stickToLast)
import qualified Lisp as L (LispVal(..), getf, fromSexp, printSexp, atom, fromLispList)
import qualified Measure as M (measuresWithBeats, M(), Ms, measuresUntilTime, Score)
import qualified AbstractScore as A (Score)
import qualified Lily as L (showLily)
import qualified Enp as E (voice2sexp)
import MeasureToEnp (vToEnp)
import MeasureToLily (vToLily)
import Data.Maybe (fromMaybe)
import Data.Either.Unwrap (fromRight)

buildMeasureFromLisp :: L.LispVal -> L.LispVal -> M.M
buildMeasureFromLisp (L.LispList [L.LispInteger n,L.LispInteger d])
                         (L.LispList [L.LispInteger tu,L.LispInteger t]) =
                         head (M.measuresWithBeats [(n,d)] [(tu,fromInteger t)])
buildMeasureFromLisp a b = error $ "buildMeasureFromLisp " ++ show a ++ " " ++ show b

ensureListOfLists :: L.LispVal -> T.Err L.LispVal
ensureListOfLists (L.LispList []) = Left "ensureListOfLists: empty list"
ensureListOfLists (L.LispList xs@(x:_)) | L.atom x = Right $ L.LispList [L.LispList xs]
                                      | otherwise = Right $ L.LispList xs
ensureListOfLists x = Left $ "ensureListOfLists: not a list: " ++ show x

ensureListOfIntegers :: Num a => L.LispVal -> T.Err [a]
ensureListOfIntegers (L.LispList xs) =
    mapM ensureInt xs
    where ensureInt (L.LispInteger x) = Right $ fromInteger x
          ensureInt v = Left $ "ensureInt: " ++ show v
ensureListOfIntegers x = Left $ "ensureListOfIntegers: " ++ show x

ensureList :: L.LispVal -> L.LispVal
ensureList x@(L.LispList _) = x
ensureList x              = L.LispList [x]

ensureList2 :: L.LispVal -> L.LispVal
ensureList2 x@(L.LispList (L.LispList _ : _)) = x
ensureList2 x@_                           = L.LispList [x]

measureStream' :: (L.LispVal, L.LispVal) -> M.Ms
measureStream' (ts, metro) = zipWith buildMeasureFromLisp
                               (U.stickToLast (L.fromLispList ts))
                               (U.stickToLast (L.fromLispList metro))

getTimeSignatures :: L.LispVal -> T.Err L.LispVal
getTimeSignatures x = case L.getf x (L.LispKeyword "TIME-SIGNATURES") of
  Just s -> ensureListOfLists s
  Nothing -> Left "Could not find :time-signatures"

getMetronomes :: L.LispVal -> T.Err L.LispVal
getMetronomes x = case L.getf x (L.LispKeyword "METRONOMES") of
  Just s -> ensureListOfLists s
  Nothing -> Left "Could not find :metronomes"

getMaxDiv :: L.LispVal -> T.Err [T.WInt]
getMaxDiv s =  case L.getf s (L.LispKeyword "MAX-DIV") of
  Just x  -> ensureListOfIntegers $ ensureList x
  Nothing -> Left "Could not find :max-div"

getForbDivs :: L.LispVal -> T.Err [[T.WInt]]
getForbDivs s =  case L.getf s (L.LispKeyword "FORBIDDEN-DIVS") of
  Just x  -> mapM ensureListOfIntegers (L.fromSexp (ensureList2 x))
  Nothing -> Left "Could not find :forbidden-divs"

addInlineOptions :: O.Options -> L.LispVal -> T.Err O.Options
addInlineOptions opts input = do
  maxdiv <- getMaxDiv input
  forbdivs <- getForbDivs input
  ts <- getTimeSignatures input
  ms <- getMetronomes input
  return $ opts { O.optMaxDiv = maxdiv
                , O.optForbiddenDivs = forbdivs
                , O.optTimeSignatures = ts
                , O.optMetronomes = ms }

measuresUntilTime :: T.Time -> M.Ms -> T.Err M.Ms
measuresUntilTime a b = Right $ M.measuresUntilTime b a

getSimple :: L.LispVal -> T.Err L.LispVal
getSimple x = fromMaybe (Left "Could not find :simple") (fmap Right (L.getf x (L.LispKeyword "SIMPLE")))

unwrapLeft :: A.Score (T.Err M.Ms) -> T.Err M.Score
unwrapLeft = Right . fmap fromRight

scoreToLily :: M.Score -> String
scoreToLily = L.showLily . fmap vToLily

scoreToEnp :: M.Score -> String
scoreToEnp = L.printSexp . fmap (E.voice2sexp . vToEnp)
