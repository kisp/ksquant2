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

(in-package #:common-lisp-user)

(defun input-files ()
  (sort (directory "shell-tests/*.lisp")
        #'string< :key #'file-namestring))

(defun read-input-file (path)
  (with-open-file (in path)
    (values (read in) (read in))))

(defun run-test (data expected)
  (with-open-file (out "tmp" :direction :output :if-exists :supersede)
    ;; (with-standard-io-syntax
    ;;   (write data :stream out))
    (prin1 data out))
  (let ((process
          (run-shell-command "./dist/build/ksquant2/ksquant2 <tmp >out")))
    (if (not process)
        (let ((result
                (with-open-file (in "out") (read in))))
          (values (equal expected result)
                  result))
        (values nil "error"))))

(defun report-on-file (path)
  (multiple-value-bind (data expected)
      (read-input-file path)
    (multiple-value-bind (success result)
        (run-test data expected)
      (format t "~A~50T~A~%" (file-namestring path) success)
      (unless success
        (format t "expected: ~S~%but got ~S~%" expected result))
      success)))

(let* ((failures (count-if #'null
                  (mapcar 'report-on-file
                    (or ext:*args*
                        (input-files)))))
       (success (zerop failures)))
  (format t "~D failure~:P~%" failures)
  (finish-output)
  (exit (if success 0 1)))
