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

(asdf:defsystem ksquant2
  :version "0.1.10"
  :description " Score quantization for PWGL - successor of ksquant"
  :maintainer "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :author "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :depends-on (ompw ksquant) ;temp dep on ksquant
  :components ((:module "lisp"
                        :serial t
                        :components ((:file "package")
                                     (:file "boxes")))))

(asdf:defsystem ksquant2-test
  :maintainer "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :author "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :depends-on (ksquant2)
  :components ((:module "lisp"
                        :serial t
                        :components ((:file "rt")
                                     (:file "boxes-test")))))
