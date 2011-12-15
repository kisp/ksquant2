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
    ,completeToScore
    ,mapVoices)
where
import Lisp

data Score a = Score { scoreParts :: [Part a] }
           deriving Show
data Part a = Part { partVoices :: [Voice a] }
           deriving Show
data Voice a = Voice { voiceItems :: [a] }
           deriving Show

class AbstractScore c a where
    completeToScore :: c a -> Score a
    mapVoices :: (Voice a -> Voice b) -> c a -> c b

instance AbstractScore Score a where
    completeToScore = id
    mapVoices f x = Score $ map (mapVoices f) (scoreParts x)

instance AbstractScore Part a where
    completeToScore x = Score [x]
    mapVoices f x = Part $ map (mapVoices f) (partVoices x)

instance AbstractScore Voice a where
    completeToScore x = completeToScore (Part [x])
    mapVoices f = f

-- Functor
instance Functor Score where
    fmap f x = Score $ map (fmap f) (scoreParts x)

instance Functor Part where
    fmap f x = Part $ map (fmap f) (partVoices x)

instance Functor Voice where
    fmap f x = Voice $ f `fmap` voiceItems x

-- Sexp
instance (Sexp a) => Sexp (Score a) where
    toSexp s = LispList $ map toSexp (scoreParts s)
    fromSexp _ = error "fromSexp Sexp (Score a) not implemented"

instance (Sexp a) => Sexp (Part a) where
    toSexp s = LispList $ map toSexp (partVoices s)
    fromSexp _ = error "fromSexp Sexp (Part a) not implemented"

instance (Sexp a) => Sexp (Voice a) where
    toSexp s = LispList $ map toSexp (voiceItems s)
    fromSexp _ = error "fromSexp Sexp (Voice a) not implemented"
