(defcustom major-mode-fontify-function #'font-lock-default-fontify-region
  "Fontify function used in `major-mode-fontify-text'.")

(defun major-mode-fontify-text (mode text)
  (with-temp-buffer
    (erase-buffer)
    (insert text)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (funcall major-mode-fontify-function (point-min) (point-max) nil)
    (buffer-string)))

(defun set-text-fontified (text)
  (let ((pos 0))
    (while (setq next (next-single-property-change pos 'face text))
      (put-text-property pos next 'font-lock-face (get-text-property pos 'face text) text)
      (setq pos next))
    (add-text-properties 0  (length text) '(fontified t) text)
    text))

(defun major-mode-fontify-region (&optional mode)
  (interactive)
  (unless (region-active-p)
    (error "Region is not active"))
  (unless mode
    (setq mode (intern (completing-read "Select major-mode: "
                                        (cl-remove-duplicates
                                         (cl-remove-if-not
                                          #'symbolp
                                          (mapcar #'cdr auto-mode-alist)))
                                        nil nil nil nil nil))))
  (let ((buffer-read-only nil))
    (let ((beg (region-beginning))
          (end (region-end)))
      (insert (funcall (if font-lock-mode #'fontify-text #'identity)
                       (major-mode-fontify-text mode
                                                (prog1 (buffer-substring-no-properties beg end)
                                                  (delete-region beg end))))))))

(provide 'partial-font-lock)
