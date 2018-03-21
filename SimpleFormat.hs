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

module SimpleFormat (Score
                    , Part
                    , Voice
                    , Event(..)
                    , Events
                    , eventStart
                    , sexp2event)

where

import qualified Types as T (Time)
import qualified Lisp as L (LispVal(LispInteger, LispFloat, LispRatio, LispList, LispSymbol)
                           , n60
                           , readLisp'
                           , car
                           , cdr
                           , getf
                           , getf')
import qualified AbstractScore as A (Score, Part, Voice)

type Notes = L.LispVal
type Expressions = L.LispVal

type Score = A.Score Events
type Part = A.Part Events
type Voice = A.Voice Events

type Events = [Event]

data Event = Chord T.Time Notes Expressions
           | Rest T.Time
           deriving (Eq,Show)

-- TODO can we use point from Interval?
eventStart :: Event -> T.Time
eventStart (Chord x _ _) = x
eventStart (Rest x) = x

sexp2event :: L.LispVal -> Event
sexp2event s = sexp2event' s False

sexp2event' :: L.LispVal -> Bool -> Event
sexp2event' (L.LispInteger x) r = sexp2event' (L.LispFloat (fromInteger x)) r
sexp2event' (L.LispFloat x) r | x < 0 || r = Rest (fromRational (abs x))
                            | otherwise = Chord (fromRational x) L.n60 (L.readLisp' "()")
sexp2event' (L.LispRatio x) r | x < 0 || r = Rest (fromRational (abs x))
                            | otherwise = Chord (fromRational x) L.n60 (L.readLisp' "()")

sexp2event' xs@(L.LispList _) _
    = if foundAndT $ L.getf (L.cdr xs) (L.readLisp' ":rest")
      then sexp2event' (L.car xs) True
      else case sexp2event' (L.car xs) False of
             Rest d ->
                 Rest d
             Chord d notes expressions ->
                 Chord d notes' expressions'
                     where notes' = L.getf' (L.cdr xs) (L.readLisp' ":notes") notes
                           expressions' = L.getf' (L.cdr xs) (L.readLisp' ":expressions") expressions
    where foundAndT Nothing = False
          foundAndT (Just (L.LispSymbol "NIL")) = False
          foundAndT (Just _) = True
sexp2event' x r = error $ "sexp2event' with " ++ show x ++ " " ++ show r
