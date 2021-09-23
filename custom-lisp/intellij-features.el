(defun intellij-backspace (arg)
  (interactive "*P")
  (if (<= (current-column) (current-indentation))
      (if (= (current-indentation) (c-get-syntactic-indentation (c-guess-basic-syntax)))
	  (if (previous-line-blank-p) (moveline-up) (delete-indentation))
	(if (= (current-column) 0)
	    (if (previous-line-blank-p) (moveline-up) (delete-indentation)) (c-indent-line)))
    (backward-delete-char-untabify(prefix-numeric-value arg))))

(defun moveline-up ()
  (delete-region (line-beginning-position 0) (line-beginning-position))
  (c-indent-line))

(defun previous-line-blank-p ()
  (save-excursion
    (previous-line)
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(provide 'intellij-features)
