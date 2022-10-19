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

(defun toggle-window-frame-decorated ()
  (interactive)
  (set-frame-parameter (window-frame) 'undecorated
                       (not (frame-parameter (window-frame) 'undecorated))))

(defun compile-user-emacs-directory ()
  (interactive)
  (mapc (lambda (file)
          (byte-compile-file file)
          (if (and (version<= "28" emacs-version) (native-comp-available-p))
              (native-compile file)))
        (directory-files "~/.emacs.d/" t ".el$")))

(defun url-encode-region ()
  (interactive)
  (unless (region-active-p) (error "Region is not activated"))
  (let* ((beg (region-beginning))
         (end (region-end))
         (url (url-encode-url (buffer-substring-no-properties beg end))))
    (delete-region (region-beginning) (region-end))
    (goto-char beg)
    (insert url)))

(provide 'emacs-ext)
