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

module Types(
  InfInt
  , WInt
  , Time
  , TimeInterval
  , Div
  , DivChoices
  , DivChoicesSeq
  , BestDivsSeq
  , Err
  , Timesig
  , Tempo
  , WRat
  , QuantGrid
  )
where

type WRat = Rational

type InfInt = Integer

type WInt = Integer

type Time = WRat
type Timesig = (WInt, WInt)
type Tempo = (WInt, WRat)

type TimeInterval = (Time, Time)

type Div = WInt
type DivChoices = [Div]
type DivChoicesSeq = [DivChoices]
type BestDivsSeq = [Div]

type QuantGrid = [(WRat, WRat)]

-- | Computation result of type 'a' or error as a String.
type Err a = Either String a
