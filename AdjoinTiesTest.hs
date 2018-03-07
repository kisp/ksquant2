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

module AdjoinTiesTest
where

import Types (WRat)
import Measure
import Test.HUnit
import Data.Ratio
import Lisp
import AdjoinTies

type Dur = WRat

q, e, s :: Dur
q = 1 % 4
e = 1 % 8
s = 1 % 16

data H = N Dur
       | T Dur
         deriving Show

buildLeaf :: H -> E
buildLeaf (N x) = l x False n60 nil
buildLeaf (T x) = l x True n60 nil

buildD :: [H] -> E
buildD hs = D q rr ls
    where ls = map buildLeaf hs
          dd = sum (map dur ls)
          rr = q / dd


buildM :: [H] -> M
buildM hs = M (1,4) (4,60) (D q 1 [buildD hs])


adjt :: [H] -> [H] -> Assertion
adjt ee ii = buildM ee @=? adjoinTies (buildM ii)

adjoin1 :: Assertion
adjoin1 =
    M (4,4) (4,60)
          (D (1 % 1) (1 % 1)
           [l q False n60 nil,
            l q False n60 nil,
            l q False n60 nil,
            l q False n60 nil])
          @=?
          adjoinTies
          (M (4,4) (4,60)
           (D (1 % 1) (1 % 1)
            [l q False n60 nil,
             l q False n60 nil,
             l q False n60 nil,
             l q False n60 nil]))

adjoin2 :: Assertion
adjoin2 = [N q, N e] `adjt` [T e, N e, N e]

adjoin3 :: Assertion
adjoin3 = [N e, N s, N s] `adjt` [T s, N s, N s, N s]
