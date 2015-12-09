;;; This file is part of KSQuant2.

;;; Copyright (c) 2010 - 2011, Kilian Sprotte. All rights reserved.

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(:SIMPLE

((((0 :notes (60))
 (1 :notes (61))
 (2 :notes (62))
 (3 :notes (63))
 (4 :notes (64))
 (5 :notes (65))
 6)))

 :TIME-SIGNATURES
 ((3 4)(5 8)(1 8)(4 8)(4 8)(4 8)(4 8)(4 4)(3 4)
  (7 16)(3 8)(5 16)(5 16)(2 8)(2 8)(2 8)(4 4))


 :METRONOMES (4 60)
 :SCALE 1/4
 :MAX-DIV 4
 :FORBIDDEN-DIVS (7))

(((((1 ((1 :notes (60))))
    (1 ((1 :notes (61))))
    (1 ((1 :notes (62))))
    :time-signature
    (3 4)
    :metronome
    (4 60))
   ((1 ((1 :notes (63))))
    (1 ((1.0 :notes (63))))
    (1 ((1 :notes (64))))
    (1 ((1.0 :notes (64))))
    (1 ((1 :notes (65))))
    :time-signature
    (5 8)
    :metronome
    (4 60))
   ((1 ((1.0 :notes (65)))) :time-signature (1 8) :metronome (4 60)))))
