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

(defpackage #:subseq-algo
  (:nicknames #:sa)
  (:use #:cl #:myam #:screamer #:alexandria)
  (:shadowing-import-from :screamer :fail))

(in-package #:sa)

(defun all-index-pairs (n)
  (if (<= n 0)
      nil
      (let ((low (an-integer-betweenv 0 n))
	    (high (an-integer-betweenv 0 n)))
	(assert! (<v low high))
	(cascade-sort:cascade-sort
	 (all-values (solution (list low high)
			       (static-ordering #'linear-force)))
	 #'< #'first #'< (uncurry #'-)))))

(defun all-index-pairs2 (n)
  (labels ((rec (x y)
	     (cond
	       ((>= x n) nil)
	       ((= x y) (rec (1+ x) n))
	       (t (cons (list x y)
			(rec x (1- y)))))))
    (rec 0 n)))

(defun uncurry (f)
  (lambda (a)
    (destructuring-bind (x y)
	a
      (funcall f x y))))

(defun all-subsequences (list)
  (mapcar (uncurry (curry #'subseq list))
	  (all-index-pairs (length list))))

(defun single-cond (list pred)
  (find-if (curry #'every pred)
	   (all-subsequences list)))

(defun single-cond2 (list pred)
  (find-if (curry #'every pred)
	   (all-index-pairs (length list))
	   :key (uncurry (curry #'subseq list))))
