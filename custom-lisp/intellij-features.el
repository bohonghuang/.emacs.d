(defun intellij-backspace (arg)
  (interactive "*P")
  (if (or (region-active-p) (not (looking-back "^[[:space:]]*")))
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

(provide 'intellij-features)
