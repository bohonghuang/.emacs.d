(defun intellij-backspace (arg)
  (interactive "*P")
  (if (or (region-active-p) (not (looking-back "^[[:space:]]*" (line-beginning-position))))
      (backward-delete-char-untabify (prefix-numeric-value arg))
    (let* ((beg (point))
           (end (progn (indent-for-tab-command) (point))))
      (when (<= beg end)
        (if (save-excursion (previous-line) (line-blank-p))
            (progn (delete-region (line-beginning-position 0) (line-beginning-position)) (back-to-indentation))
          (delete-indentation))))))

(defun line-blank-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun pycharm-return (&optional arg)
  (interactive "*P")
  (if (looking-back "^[[:space:]]*" (line-beginning-position))
    (let ((arg (or arg 1))
          (indent (buffer-substring-no-properties (line-beginning-position) (point))))
      (dotimes (_ arg)
        ;; (delete-region (line-beginning-position) (point))
        (newline)
        (insert indent)))
    (call-interactively #'newline arg)))

(defun intellij-after-drag ()
  (if (region-active-p)
      (progn
        (indent-region-line-by-line (region-beginning) (region-end))
        (setq deactivate-mark nil)
        )
    (indent-for-tab-command)))

(add-hook 'drag-stuff-after-drag-hook #'intellij-after-drag)

(provide 'intellij-features)
