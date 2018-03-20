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

module AbstractScore
    (Score(scoreParts)
    ,Part(partVoices)
    ,Voice(voiceItems)
    ,singleVoice2Score)

where

import Lisp (Sexp
            , toSexp
            , fromSexp
            , LispVal(LispList)
            , mapcar'
            , mapcarUpToPlist
            , fromLispList
            , nil)

data Score a = Score { scoreParts :: [Part a] } deriving (Eq,Show)
data Part a = Part { partVoices :: [Voice a], partAttributes :: LispVal } deriving (Eq,Show)
data Voice a = Voice { voiceItems :: a } deriving (Eq,Show)

-- Functor
instance Functor Score where
    fmap f x = Score $ map (fmap f) (scoreParts x)

instance Functor Part where
    fmap f x = x { partVoices = map (fmap f) (partVoices x) }

instance Functor Voice where
    fmap f x = x { voiceItems = f . voiceItems $ x }

-- Sexp
instance (Sexp a) => Sexp (Score a) where
    toSexp s = LispList $ map toSexp (scoreParts s)
    fromSexp = Score . mapcar' fromSexp

instance (Sexp a) => Sexp (Part a) where
    toSexp s = LispList $ map toSexp (partVoices s) ++ fromLispList (partAttributes s)
    fromSexp = uncurry Part . mapcarUpToPlist fromSexp

instance (Sexp a) => Sexp (Voice a) where
    toSexp = toSexp . voiceItems
    fromSexp = Voice . fromSexp

singleVoice2Score :: a -> Score a
singleVoice2Score a = Score { scoreParts = [p] }
  where v = Voice a
        p = Part { partVoices = [v]
                 , partAttributes = nil }
