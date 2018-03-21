module Options ( Options(..), PureMain, PureMultiMain )

where

import qualified Types as T (WInt, Err)
import qualified Lisp as L (LispVal)

data Options = Options  { optVerbose        :: Bool
                        , optInputFormat    :: String
                        , optOutputFormat   :: String
                        , optMaxDiv         :: [T.WInt]
                        , optForbiddenDivs  :: [[T.WInt]]
                        , optTimeSignatures :: L.LispVal
                        , optMetronomes     :: L.LispVal
                        , optMeasureSiblingMerge :: Bool
                        }

type PureMain = Options -> String -> T.Err String

type PureMultiMain = Options -> [String] -> T.Err [String]
