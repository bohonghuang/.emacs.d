(require 'dash)

(defun line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "\\_>*$")))

(defun yank-with-indent (&optional arg)
  (interactive "*P")
  (let* ((text (with-temp-buffer
                 (insert (current-kill 0))
                 (goto-char (point-min))
                 (let ((indent most-positive-fixnum))
                   (while (not (eobp))
                     (when (not (line-empty-p))
                       (setq indent (min indent (current-indentation))))
                     (forward-line 1))
                   (goto-char (point-min))
                   (while (not (eobp))
                     (when (>= (current-indentation) indent)
                       (beginning-of-line)
                       (delete-forward-char indent))
                     (forward-line 1)))
                 (buffer-string)))
         (indent (current-indentation))
         (beg (point))
         (end (progn
                (print text)
                (kill-new text)
                (yank)
                (point))))
    (with-suppressed-message
      (replace-string "\n" (concat "\n" (-repeat indent ? )) nil beg end))))

(provide 'simple-ext)
