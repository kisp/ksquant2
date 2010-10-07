-- This file is part of KSQuant2.

-- Copyright (c) 2010, Kilian Sprotte. All rights reserved.

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

ms = M.measuresWithBeats [(5,8),(13,4)] (repeat 60)
score = A.completeToScore (A.Voice ms)
-- m = M.measuresDivideLeafs (M.measuresWithBeats [(4,4)] [60]) (repeat 1)
-- m = [(M.m (4,4) 60 (M.l (4%4) False))]
enp = fmap mToEnp score
l = fmap mToLily score

main = do
  exportLily "foo" l
  writeFile "/tmp/toll.enp" (printLisp (score2sexp enp))
  rawSystem "scp" ["/tmp/toll.enp", "plmb.local:/tmp/sc.lisp"]
