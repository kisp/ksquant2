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
module MeasureToEnpTest where
import Measure
import MeasureToEnp
import Enp
import Test.HUnit
import Data.Ratio
import Lisp (readLisp)

n60 = readLisp "(60)"
nil = readLisp "()"

mtoenp1 = TestList
           [
            Enp.Measure (4,4) (4,60) [Enp.Div 4 [Enp.Chord 1 False n60 nil]]
            ~=? let leaf = (L 1 False 0 n60 nil)
                in mToEnp
                       [(0,leaf)]
                       (M (4,4) (4,60 % 1) leaf)
           ,Enp.Measure (1,4) (4,60) [Enp.Div 1 [Enp.Chord 1 False n60 nil]]
            ~=? let leaf = (L (1%4) False 0 n60 nil)
                in mToEnp
                       [(0,leaf)]
                       (M (1,4) (4,60 % 1) leaf)           
           -- ,Enp.Measure (2,4) [Enp.Div 1 [Enp.Chord 1 False],
           --                    Enp.Div 1 [Enp.Chord 1 False,Enp.Chord 1 False]]
           --  ~=? mToEnp (m (2,4) (60 % 1)
           --                      (d (2%4) 1
           --                             [(l (1%4) False),
           --                              (d (1%4) 1 [(l (1%8) False),
           --                                          (l (1%8) False)])]))      
           -- ,Enp.Measure (2,4) [Enp.Div 1 [Enp.Chord 1 False],
           --                    Enp.Div 1 [Enp.Chord 1 False,Enp.Chord 1 False]]
           --  ~=? mToEnp (m (2,4) (60 % 1)
           --                      (d (2%4) 1 [(d (1%4) 1 [(l (1%4) False)]),
           --                                  (d (1%4) 1 [(l (1%8) False),
           --                                              (l (1%8) False)])]))           
           ]
