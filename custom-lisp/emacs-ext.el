;;;  -*- lexical-binding: t; -*-

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(defun toggle-dedicate-window ()
  (interactive)
  (let ((window (selected-window)))
    (set-window-dedicated-p window (not (window-dedicated-p window)))))

(defun compile-user-emacs-directory ()
  (interactive)
  (mapc (lambda (file)
          (byte-compile-file file)
          (if (and (version<= "28" emacs-version) (native-comp-available-p))
              (native-compile file)))
        (directory-files "~/.emacs.d/" t ".el$")))

(provide 'emacs-ext)
