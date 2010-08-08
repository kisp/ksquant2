;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:ksquant2)

(define-menu ksquant2)
(in-menu ksquant2)

(define-box simple2score ((simple (0 1 2 3))
			  &key
			  (time-signatures (4 4))
			  (metronomes (4 60))
			  (scale 1/4)
			  (max-div 8)
			  (forbidden-divs (7)))
  :non-generic t
  (let ((*default-pathname-defaults* (asdf:component-pathname (asdf:find-system :ksquant2))))
    (with-open-file (out "/tmp/ksquant2" :direction :output :if-exists :supersede)
      (with-standard-io-syntax
	(write
	 `(:simple
	   ,simple
	   :time-signatures ,time-signatures
	   :metronomes ,metronomes
	   :scale ,scale
	   :max-div ,max-div
	   :forbidden-divs ,forbidden-divs)
	 :stream out)))
    (assert (zerop (sys:call-system (format nil "'~A' </tmp/ksquant2 >/tmp/ksquant2.out"
					    (namestring (merge-pathnames "bin/ksquant2-kernel"))))))
    (with-open-file (in "/tmp/ksquant2.out")
      (ccl::make-score (read in)))))

(install-menu ksquant2)
