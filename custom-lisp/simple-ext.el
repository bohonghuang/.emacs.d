;;; -*- lexical-binding: t -*-

(defun filter-initials-region (&optional beg end)
  (interactive)
  (let ((letters nil)
        (beg (or beg (region-beginning)))
        (end (or end (region-end))))
    (goto-char beg)
    (save-restriction
      (narrow-to-region beg end)
      (while
          (progn
            (push (upcase (char-after)) letters)
            (ignore-errors
              (search-forward-regexp "[ \\-][A-z]")
              (backward-char)
              t))))
    (delete-region beg end)
    (insert (concat (reverse letters)))))

(defun filter-and-upcase-initials-region (&optional beg end)
  (interactive)
  (filter-initials-region beg end)
  (upcase-region (region-beginning) (region-end)))

(defun reverse-chars-region (beg end)
 "Reverse characters between BEG and END."
 (interactive "r")
 (let ((region (buffer-substring beg end)))
   (delete-region beg end)
   (insert (nreverse region))))

(provide 'simple-ext)
