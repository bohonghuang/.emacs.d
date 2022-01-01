(require 'project)

(defvar run-command nil)

(defun project-run ()
  (interactive)
  (let ((compile-command run-command))
    (call-interactively #'project-compile)))

(put 'run-command 'safe-local-variable #'stringp)

(provide 'project-ext)
