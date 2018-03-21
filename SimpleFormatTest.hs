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

module SimpleFormatTest

where

import qualified SimpleFormat as SF1 ( Event(Chord, Rest), sexp2event )
import qualified Lisp as L ( readLisp' )
import Test.HUnit ( Test(TestList), (~=?) )

sexp2event1 :: Test
sexp2event1 = TestList
              [
               SF1.Rest 1.0 ~=? SF1.sexp2event (L.readLisp' "(1.0 :REST T)")
              ,SF1.Rest 1.0 ~=? SF1.sexp2event (L.readLisp' "-1.0")
              ,SF1.Chord 1.0 (L.readLisp' "(60)") (L.readLisp' "()")
               ~=? SF1.sexp2event (L.readLisp' "1.0")
              ,SF1.Chord 0.0 (L.readLisp' "(60)") (L.readLisp' "()")
               ~=? SF1.sexp2event (L.readLisp' "0.0")
              ,SF1.Chord 0.0 (L.readLisp' "(60)") (L.readLisp' "()")
               ~=? SF1.sexp2event (L.readLisp' "-0.0")
              ,SF1.Chord 0.0 (L.readLisp' "(60)") (L.readLisp' "()")
               ~=? SF1.sexp2event (L.readLisp' "0")
              ,SF1.Chord 0.0 (L.readLisp' "(67)") (L.readLisp' "()")
               ~=? SF1.sexp2event (L.readLisp' "(0 :NOTES (67))")
              ,SF1.Chord 3.0 (L.readLisp' "(67)") (L.readLisp' "(:ACCENT)")
               ~=? SF1.sexp2event (L.readLisp' "(3 :NOTES (67) :EXPRESSIONS (:ACCENT))")
              ,SF1.Chord 3.0 (L.readLisp' "(60)") (L.readLisp' "(:ACCENT)")
               ~=? SF1.sexp2event (L.readLisp' "(3 :EXPRESSIONS (:ACCENT))")
              ,SF1.Rest 0.0 ~=? SF1.sexp2event (L.readLisp' "(0.0 :REST T)")
              ,SF1.Rest 0.0 ~=? SF1.sexp2event (L.readLisp' "(0 :REST T)")
              ]
