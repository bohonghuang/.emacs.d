;;; -*- lexical-binding: t -*-

(require 'project)

(defvar run-command nil)

(defun project-run ()
  (interactive)
  (let ((compile-command run-command))
    (call-interactively #'project-compile)))

(defun project-run-or-quickrun ()
  (interactive)
  (if (project-current)
      (project-run)
    (quickrun)))

(put 'run-command 'safe-local-variable #'stringp)

(provide 'project-ext)
