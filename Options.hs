module Options ( Options(..), PureMain, PureMultiMain )
where

import Types (WInt, Err)
import Lisp (LispVal)

data Options = Options  { optVerbose        :: Bool
                        , optInputFormat    :: String
                        , optOutputFormat   :: String
                        , optMaxDiv         :: [WInt]
                        , optForbiddenDivs  :: [[WInt]]
                        , optTimeSignatures :: LispVal
                        , optMetronomes     :: LispVal
                        , optMeasureSiblingMerge :: Bool
                        }

type PureMain = Options -> String -> Err String

type PureMultiMain = Options -> [String] -> Err [String]
