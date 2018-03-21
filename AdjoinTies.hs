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

module AdjoinTies (adjoinTies)

where

import qualified Measure as M ( M(M), E(L,D) )
import Data.Ratio ((%))
import qualified Lisp as L ( nil, n60 )
--import Debug.Trace

adjoinTies' :: M.M -> M.M
adjoinTies' x
  | x == M.M (1,4) (4,60 % 1)
               (M.D (1 % 4) (1 % 1)
                [M.D (1 % 4) (2 % 3)
                 [M.L (1 % 8) True 0 L.n60 L.nil
                 ,M.L (1 % 8) False 0 L.n60 L.nil
                 ,M.L (1 % 8) False 0 L.n60 L.nil]]) =
    M.M (1,4) (4,60 % 1)
          (M.D (1 % 4) (1 % 1)
                 [M.D (1 % 4) (2 % 3)
                        [M.L (2 % 8) False 0 L.n60 L.nil
                        ,M.L (1 % 8) False 0 L.n60 L.nil]])
  | x == M.M (1,4) (4,60 % 1)
    (M.D (1 % 4) (1 % 1)
     [M.D (1 % 4) (1 % 1)
      [M.L (1 % 16) True 0 L.n60 L.nil
      ,M.L (1 % 16) False 0 L.n60 L.nil
      ,M.L (1 % 16) False 0 L.n60 L.nil
      ,M.L (1 % 16) False 0 L.n60 L.nil]]) =
    M.M (1,4) (4,60 % 1)
          (M.D (1 % 4) (1 % 1)
           [M.D (1 % 4) (1 % 1)
            [M.L (1 % 8) False 0 L.n60 L.nil
            ,M.L (1 % 16) False 0 L.n60 L.nil
            ,M.L (1 % 16) False 0 L.n60 L.nil]])
  | otherwise = x

adjoinTies :: M.M -> M.M
--adjoinTies = traceShowId . adjoinTies' . traceShowId
adjoinTies = adjoinTies'
