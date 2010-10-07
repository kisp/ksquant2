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
module SimpleFormatTest where
import SimpleFormat
import Lisp (readLisp)
import Test.HUnit

sexp2event1 = TestList
              [
               Rest 1.0 ~=? sexp2event (readLisp "(1.0 :REST T)")
              ,Rest 1.0 ~=? sexp2event (readLisp "-1.0")
              ,Chord 1.0 (readLisp "(60)") (readLisp "()")
               ~=? sexp2event (readLisp "1.0")
              ,Chord 0.0 (readLisp "(60)") (readLisp "()")
               ~=? sexp2event (readLisp "0.0")
              ,Chord 0.0 (readLisp "(60)") (readLisp "()")
               ~=? sexp2event (readLisp "-0.0")
              ,Chord 0.0 (readLisp "(60)") (readLisp "()")
               ~=? sexp2event (readLisp "0")
              ,Chord 0.0 (readLisp "(67)") (readLisp "()")
               ~=? sexp2event (readLisp "(0 :NOTES (67))")
              ,Chord 3.0 (readLisp "(67)") (readLisp "(:ACCENT)")
               ~=? sexp2event (readLisp "(3 :NOTES (67) :EXPRESSIONS (:ACCENT))")
              ,Chord 3.0 (readLisp "(60)") (readLisp "(:ACCENT)")
               ~=? sexp2event (readLisp "(3 :EXPRESSIONS (:ACCENT))")
              ,Rest 0.0 ~=? sexp2event (readLisp "(0.0 :REST T)")
              ,Rest 0.0 ~=? sexp2event (readLisp "(0 :REST T)")
              ]
