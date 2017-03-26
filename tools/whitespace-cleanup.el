#! /bin/sh
":"; exec emacs --no-site-file --script "$0" -- "$@" # -*-emacs-lisp-*-
(setq argv (cdr argv))

(setq find-file-hook (remove 'vc-find-file-hook find-file-hook))

(setq-default indent-tabs-mode nil)

(defun cleanup-file (file)
  (unless (file-exists-p file)
    (error "File does not exist: %s" file))
  (message "Opening %s..." file)
  (find-file file)
  (if (eql buffer-file-coding-system 'no-conversion)
      (message "Binary file!")
    (progn
      (setq indent-tabs-mode nil)       ;maybe we do not need this
      (whitespace-cleanup)
      (save-buffer)))
  (kill-buffer))

(dolist (arg argv)
  (cleanup-file arg))
