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

(defun input-files ()
  (directory "shell-tests/*.lisp"))

(defun read-input-file (path)
  (with-open-file (in path)
    (values (read in) (read in))))

(defun run-test (data expected)
  (with-open-file (out "tmp" :direction :output :if-exists :supersede)
    ;; (with-standard-io-syntax
    ;;   (write data :stream out))
    (prin1 data out))
  (and (not (run-shell-command "./main <tmp >out"))
       (equal expected (with-open-file (in "out") (read in)))))

(defun report-on-file (path)
  (multiple-value-bind (data expected)
      (read-input-file path)
    (let ((success (run-test data expected)))
      (format t "~A~50T~A~%" (file-namestring path) success)
      success)))

(let ((success (every #'identity (mapcar 'report-on-file (input-files)))))
  (finish-output)
  (exit (if success 0 1)))
