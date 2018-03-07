module MainUtils
where

import Types (Err, WInt, Time)
import Options ( Options(..) )
import Utils (stickToLast, appendNewline)
import Lisp (LispVal(..), getf, fromSexp, printSexp)
import qualified Measure as M (measuresWithBeats, M(), Ms, measuresUntilTime, Score)
import qualified AbstractScore as A
import qualified Lily as L (showLily)
import qualified Enp as E (voice2sexp)
import MeasureToEnp (vToEnp)
import MeasureToLily (vToLily)
import Lisp (atom, fromLispList)
import Data.Maybe (fromMaybe)
import Data.Either.Unwrap (fromRight)
import Control.Monad (liftM)

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
