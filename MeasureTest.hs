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

module MeasureTest

where

import Measure ( M(M), E(D), l, measuresWithBeats, measureDur )
import Test.HUnit (Test(TestList), (~=?))
import Data.Ratio ( (%) )
import Lisp (n60, nil)

measure1 :: Test
measure1 = TestList
           [[M (4,4) (4,60 % 1)
             (D (1 % 1) (1 % 1)
              [l (1 % 4) False n60 nil,
               l (1 % 4) False n60 nil,
               l (1 % 4) False n60 nil,
               l (1 % 4) False n60 nil])]
            ~=?  measuresWithBeats [(4,4)] [(4,60)]
           ,[M (3,4) (4,60 % 1)
             (D (3 % 4) (1 % 1)
              [l (1 % 4) False n60 nil,
               l (1 % 4) False n60 nil,
               l (1 % 4) False n60 nil])]
            ~=?  measuresWithBeats [(3,4)] [(4,60)]
           ,[M (5,8) (4,60 % 1)
             (D (5 % 8) (1 % 1)
              (replicate 5 (l (1 % 8) False n60 nil)))]
            ~=?  measuresWithBeats [(5,8)] [(4,60)]
           ,[1,2,3,4,5]
            ~=?  map measureDur (measuresWithBeats [(1,4),(2,4),(3,4),(4,4),(5,4)] [(4,60),(4,60),(4,60),(4,60),(4,60)])
           ,[1%2,2%2,3%2,4%2,5%2]
            ~=?  map measureDur (measuresWithBeats [(1,4),(2,4),(3,4),(4,4),(5,4)] [(4,120),(4,120),(4,120),(4,120),(4,120)])
           ,[1%2,2%2,3%2,4%2,5%2]
            ~=?  map measureDur (measuresWithBeats [(1,4),(2,4),(3,4),(4,4),(5,4)] [(8,60),(8,60),(8,60),(8,60),(8,60)])
           ,[1%4,2%4,3%4,4%4,5%4]
            ~=?  map measureDur (measuresWithBeats [(1,4),(2,4),(3,4),(4,4),(5,4)] [(8,120),(8,120),(8,120),(8,120),(8,120)])
           ,[1%2,2%2,3%2,4%2,5%2]
            ~=?  map measureDur (measuresWithBeats [(1,8),(2,8),(3,8),(4,8),(5,8)] [(4,60),(4,60),(4,60),(4,60),(4,60)])
           ,[1%4,2%4,3%4,4%4,5%4]
            ~=?  map measureDur (measuresWithBeats [(1,8),(2,8),(3,8),(4,8),(5,8)] [(4,120),(4,120),(4,120),(4,120),(4,120)])
           ,[1%4,2%4,3%4,4%4,5%4]
            ~=?  map measureDur (measuresWithBeats [(1,8),(2,8),(3,8),(4,8),(5,8)] [(8,60),(8,60),(8,60),(8,60),(8,60)])
           ,[1%8,2%8,3%8,4%8,5%8]
            ~=?  map measureDur (measuresWithBeats [(1,8),(2,8),(3,8),(4,8),(5,8)] [(8,120),(8,120),(8,120),(8,120),(8,120)])
           ]
