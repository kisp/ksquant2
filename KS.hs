module KS where

import Enp
import qualified Measure as M
import Lily
import MeasureToLily
import MeasureToEnp
import Lisp
import System.IO
import System.Cmd
import Data.Ratio
import qualified AbstractScore as A

ms = M.measures_with_beats [(5,8),(13,4)] (repeat 60)
score = A.completeToScore (A.Voice ms)
-- m = M.measures_divide_leafs (M.measures_with_beats [(4,4)] [60]) (repeat 1)
-- m = [(M.m (4,4) 60 (M.l (4%4) False))]
enp = fmap m_to_enp score
l = fmap m_to_lily score

main = do
  exportLily "foo" l
  writeFile "/tmp/toll.enp" (printLisp (score2sexp enp))
  rawSystem "scp" ["/tmp/toll.enp", "plmb.local:/tmp/sc.lisp"]
