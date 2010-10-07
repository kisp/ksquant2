;;; This file is part of KSQuant2.

;;; Copyright (c) 2010, Kilian Sprotte. All rights reserved.

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
(in-package #:common-lisp-user)

(defun input (simple)
  `(:SIMPLE ,simple
	    :TIME-SIGNATURES (4 4)
	    :METRONOMES (4 60)
	    :MAX-DIV ,(1+ (random 15))
	    :FORBIDDEN-DIVS nil))

(defun voice->score (voice)
  (list (list voice)))

(defun generate-simple ()
  (voice->score
   (delete-duplicates
    (cons (float 0)
	  (sort
	   (loop
	      with rests = (random 2)
	      repeat (+ 10 (random 300))
	      collect (* (if (zerop rests)
			     (if (zerop (random 2)) 1 -1)
			     1)
			 (float (/ (random 1000) 100))))
	   #'<
	   :key #'abs))
    :test #'=
    :key #'abs)))

(defun write-input (simple)
  (with-open-file (out "input" :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (write (input simple) :stream out))))

(defun test-it ()
  (sb-posix:chdir "/home/paul/ksquant2")
  (sb-ext:process-exit-code
   (sb-ext:run-program "./Main" nil :input "input")))

(defun test-input (simple)
  (write-input simple)
  (test-it))
