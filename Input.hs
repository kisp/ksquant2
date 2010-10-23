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

{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Input where
import qualified Interval as Iv

type Time = Float

data Event = Event Time Time
             deriving Show

instance Iv.Interval Event Time where
    start (Event start _     ) = start
    end   (Event _     end   ) = end

instance Iv.Point Time Time where
    point x = x

-----------------------------

data QEvent = QEvent Rational Rational [Event]
              deriving Show

instance Iv.Interval QEvent Rational where
    start (QEvent start _   _) = start
    end   (QEvent _     end _) = end
