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

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module AbstractScore
    (Score(..)
    ,Part(..)
    ,Voice(..)
    ,mapVoices)
where

data Score a = Score { scoreParts :: [Part a] }
data Part a = Part { partVoices :: [Voice a] }
data Voice a = Voice { voiceItems :: a }

mapVoices :: Functor f => (a -> b) -> f a -> f b
mapVoices = fmap

-- Functor
instance Functor Score where
    fmap f x = Score $ map (fmap f) (scoreParts x)

instance Functor Part where
    fmap f x = Part $ map (fmap f) (partVoices x)

instance Functor Voice where
    fmap f x = Voice . f . voiceItems $ x
