;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:ksquant2)

(define-menu ksquant2 :print-name "*KSQuant2")
(in-menu ksquant2)

(defun getpid ()
  #+win32 (win32::get-current-process-id)
  #-win32 (sys::getpid))

(defun format-time-stamp (&optional (utime (get-universal-time)))
  (multiple-value-bind (sc mn hr d m y day) (decode-universal-time utime)
    (declare (ignore day))
    (format nil "~2,'0D~2,'0D~2,'0D_~2,'0D~2,'0D~2,'0D"
	    (mod y 100) m d hr mn sc)))

(defun report-bug (code sf-path enp-path err-path)
  (macrolet ((form (form)
	       `(progn
		  (princ ',form out)
		  (write-string " => " out)
		  (princ ,form out)
		  (terpri out))))
    (with-open-file (out "/tmp/report" :direction :output
			 :if-exists :supersede)
      (format out "time: ~A~%" (format-time-stamp))
      (format out "code: ~A~%" code)
      (format out "lw: ~A ~A~%"
	      (lisp-implementation-type) (lisp-implementation-version))
      (format out "ksquant version: ~A~%"
	      (asdf:component-version (asdf:find-system :ksquant)))
      (format out "ksquant2 version: ~A~%"
	      (asdf:component-version (asdf:find-system :ksquant2)))
      (format out "pwgl version: ~A~%" (sys::pwgl-version-text))
      (form (probe-file "/usr/lib/libSystem.B.dylib"))
      (form (probe-file "/opt/local/lib/libgmp.10.dylib"))
      (form (probe-file "/usr/lib/libgcc_s.1.dylib"))
      (form (probe-file "/opt/"))
      (form (with-output-to-string (*standard-output*)
	      (sys::call-system-showing-output "uname -a")))))
  (unless (probe-file enp-path)
    (with-open-file (out enp-path :direction :output)
      (write-line "did not exist" out)))
  (unless (probe-file err-path)
    (with-open-file (out err-path :direction :output)
      (write-line "did not exist" out)))
  (unless (probe-file sf-path)
    (with-open-file (out sf-path :direction :output)
      (write-line "did not exist" out)))
  (sys::call-system
   (format nil "tar cfz /tmp/report.tgz /tmp/report ~A ~A ~A"
	   sf-path enp-path err-path))
  (capi:display-message
   (format
    nil
    "Report has been generated. Please choose a place where to save it. ~
   ~%Please send then the report file as an attachment to me by mail ~
    (sending of the patch is not absolutely necessary)."))
  (let ((path (capi:prompt-for-file
	       "Where to save report?"
	       :operation :save
	       :filters nil
	       :pathname (merge-pathnames
			  (make-pathname :name (format nil "ksquant2-report-~A"
						       (format-time-stamp))
					 :type "tgz"
					 :directory '(:relative "Desktop"))
			  (user-homedir-pathname)))))
    (unless path
      (capi:display-message "Saving of report cancelled.")
      (abort))
    (sys::call-system (format nil "mv /tmp/report.tgz '~A'" path))))

(define-box simple2score ((simple (0 1 2 3))
			  &key
			  (time-signatures (4 4))
			  (metronomes (4 60))
			  (max-div 8)
			  (forbidden-divs (7)))
  :non-generic t
  (let* ((*default-pathname-defaults* (asdf:component-pathname
				       (asdf:find-system :ksquant2)))
	 (sf-path (format nil "/tmp/ksquant2-~A" (getpid)))
	 (enp-path (format nil "/tmp/ksquant2-out-~A" (getpid)))
	 (err-path (format nil "/tmp/ksquant2-err-~A" (getpid)))
	 (kernel-path (namestring (merge-pathnames "kernel")))
	 (simple (ksquant::simple-change-type* :score simple)))
    (unless (probe-file kernel-path)
      (error "Cannot find ksquant2 kernel! Have you installed ~
	      the binary version? Or compiled the kernel yourself?"))
    (with-open-file (out sf-path :direction :output :if-exists :supersede)
      (with-standard-io-syntax
	(write
	 `(:simple
	   ,simple
	   :time-signatures ,time-signatures
	   :metronomes ,metronomes
	   :max-div ,max-div
	   :forbidden-divs ,forbidden-divs)
	 :stream out)))
    (let ((code (ccl::with-message-dialog "KSQuant2: kernel..."
		  (sys:call-system
		   (format nil "'~A' <~A >~A 2>~A"
			   kernel-path sf-path enp-path err-path)))))
      (unless (zerop code)
	(let ((err-message (or (ignore-errors (with-open-file (in err-path)
						(read-line in)))
			       "<no message>")))
	  (if (capi:prompt-for-confirmation
	       (format nil "Error: ~S~2%The ksquant2 kernel has exited ~
			    with an error. If you think this is a bug, ~
			    please report it (by answering No). ~
			 ~2%Do you want to try with ksquant version 1 instead?"
		       err-message)
	       :default-button :yes)
	      (return-from simple2score
		(ksquant:simple2score simple
				      :time-signatures time-signatures
				      :metronomes metronomes
				      :scale 1/4
				      :max-div max-div
				      :forbidden-divs forbidden-divs))
	      (progn
		(when (capi:prompt-for-confirmation
		       "Do you want to generate a bug report?"
		       :default-button :no)
		  (report-bug code sf-path enp-path err-path))
		(abort))))))
    (with-open-file (in enp-path)
      (ccl::adjoin-ties (ccl::make-score (read in))))))

(install-menu ksquant2)
