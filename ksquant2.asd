;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp  -*-

(asdf:defsystem ksquant2
  :version "0.1.1"
  :description " Score quantization for PWGL - successor of ksquant"
  :maintainer "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :author "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :depends-on (ompw)
  :serial t
  :components ((:module "lisp"
			:components ((:file "package")
				     (:file "boxes")))))
