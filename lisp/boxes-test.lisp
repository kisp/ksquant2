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
(defpackage #:ksquant2-test
  (:use #:cl #:ksquant2 #:regression-test))

(in-package #:ksquant2-test)

(rem-all-tests)

(deftest dummy
  (+ 1 2)
  3)

(deftest simple.1
  (ccl:enp-score-notation (simple2score '(0 1 2 3)) :include nil)
  (((((1 (1)) (1 (1)) (1 (1)) (1 (-1)))))))

(defun invoke-with-tmp-dir-stub (thunk)
  (let* ((tmp-dir (merge-pathnames
		   (make-pathname :directory `(:relative ,(princ-to-string (random 1000000))))
		   (ksquant2::tmp-dir)))
	 (stub (lambda () tmp-dir))
	 (orig (symbol-function 'ksquant2::tmp-dir)))
    (assert (not (probe-file tmp-dir)))
    (unwind-protect
	 (progn
	   (ensure-directories-exist tmp-dir)
	   (setf (symbol-function 'ksquant2::tmp-dir) stub)
	   (funcall thunk))
      (setf (symbol-function 'ksquant2::tmp-dir) orig)
      (cl-fad:delete-directory-and-files tmp-dir))))

(defmacro with-tmp-dir-stub (&body body)
  "Ensure that KSQUANT2::TMP-DIR returns a fresh and empty dir that
  exists only for BODY's dynamic extent."
  `(invoke-with-tmp-dir-stub (lambda () ,@body)))

(deftest tmp-dir-internal.1
  (with-tmp-dir-stub
    (directory (merge-pathnames (make-pathname :name :wild :type :wild)
				(ksquant2::tmp-dir))))
  nil)

(deftest tmp-dir-internal.2
  (with-tmp-dir-stub
    (not (probe-file (ksquant2::tmp-dir))))
  nil)

(deftest tmp-dir-internal.3
  (let ((tmp-dir (with-tmp-dir-stub (ksquant2::tmp-dir))))
    (not (probe-file tmp-dir)))
  t)

(deftest tmp-dir-internal.4
  (let ((tmp-dir (with-tmp-dir-stub
		   (with-open-file (*standard-output*
				    (merge-pathnames "foo" (ksquant2::tmp-dir))
				    :direction :output)
		     (print "foo"))
		   (ksquant2::tmp-dir))))
    (not (probe-file tmp-dir)))
  t)

(deftest tmp-dir.1
  (with-tmp-dir-stub
    (simple2score '(0 1 2 3))
    (directory (merge-pathnames (make-pathname :name :wild :type :wild)
				(ksquant2::tmp-dir))))
  nil)
