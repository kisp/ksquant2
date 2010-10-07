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
(progn
  (load "/tmp/haskell-mode-2.4/haskell-site-file.el")

  (require 'flymake)

  (defun flymake-hslint-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
			 'flymake-create-temp-inplace))
	   (local-file  (file-relative-name
			 temp-file
			 (file-name-directory buffer-file-name))))
      (list "hslint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.l?hs\\'" flymake-hslint-init))
  (setq inferior-haskell-find-project-root nil)
  (add-hook 'haskell-mode-hook 'flymake-mode))
