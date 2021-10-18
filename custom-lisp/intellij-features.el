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

(defun pycharm-backspace (&optional arg)
  (interactive "*P")
  (if (region-active-p)
      (delete-region (region-beginning) (region-end))
    (python-indent-dedent-line-backspace (or arg 1))))

(defun intellij-after-drag ()
  (if (region-active-p)
      (progn
        (indent-region-line-by-line (region-beginning) (region-end))
        (setq deactivate-mark nil)
        )
    (indent-for-tab-command)))

(defun line-indentation ()
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (back-to-indentation)
      (- (point) beg))))

(defun intellij-left-bracket ()
  (interactive)
  (unless (catch 'exit
            (if (region-active-p)
                (let ((begin (region-beginning))
                      (end (region-end)))
                  (goto-char end)
                  (if (line-blank-p)
                      (progn
                        (indent-for-tab-command)
                        (insert "}"))
                    (newline-and-indent)
                    (insert "}"))
                  (goto-char begin)
                  (if (looking-back "^[[:space:]]*")
                      (if (save-excursion (previous-line)
                                           (end-of-line)
                                           (looking-back ")[[:space:]]*"))
                          (progn
                            (previous-line)
                            (end-of-line)
                            (delete-horizontal-space)
                            (insert " {"))
                        (open-line 1)
                        (insert "{"))
                    (insert "{")
                    (unless (looking-at "[[:space:]]*$") (newline-and-indent)))
                  (indent-region begin end)
                  t)
              (if (looking-at "[[:space:]]*$")
                  (if (save-excursion (beginning-of-line)
                                      (back-to-indentation)
                                      (looking-at "\\(if\\|\\(\\(\\}[[:space:]]*\\)*else\\)\\|for\\|while\\)"))
                      (let ((indent-current (line-indentation))
                            (indent-next (save-excursion (ignore-errors (next-line) (current-line-indent))))
                            (beg (point))
                            (line-count 0))
                        (if (and indent-next (>= indent-current indent-next))
                            (throw 'exit nil)
                          (save-excursion
                            (while (catch 'continue
                                     (ignore-errors
                                       (next-line)
                                       (throw 'continue (> (line-indentation) indent-current)))
                                     nil)
                              (setq line-count (+ line-count 1))))
                          (if (= line-count 0)
                              (throw 'exit nil)
                            (insert "{")
                            (next-line line-count)
                            (end-of-line)
                            (newline)
                            (insert "}")
                            (indent-for-tab-command)
                            (previous-line)
                            (end-of-line)
                            t))))
                (insert "{")
                (newline-and-indent)
                (end-of-line)
                (let ((beg (point)))
                  (newline)
                  (insert "}")
                  (indent-for-tab-command)
                  (goto-char beg)
                  t))))
    (sp-insert-pair "{")))

(defun intellij-return (&optional arg)
    (interactive "P")
    (if (and (looking-back "{[[:space:]]*" (line-beginning-position))
             (looking-at "[[:space:]]*}"))
        (progn
          (newline-and-indent (+ (or arg 1) 1))          
          (previous-line)
          (indent-for-tab-command))
      (call-interactively #'newline)))


(add-hook 'drag-stuff-after-drag-hook #'intellij-after-drag)

(provide 'intellij-features)
