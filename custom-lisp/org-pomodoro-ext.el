(require 'org)

(defmacro org-pomodoro-with-current-drawer (&rest body)
  `(pcase (marker-buffer org-clock-marker)
     (`nil (message "Cannot find running pomodoro, abort logging."))
     (buffer (save-excursion
               (with-current-buffer buffer
                 (if (catch 'exit
                       (goto-char org-clock-marker)
                       (org-back-to-heading t)
                       (let* ((beg (line-beginning-position))
                              (end (save-excursion (outline-next-heading) (point)))
                              (drawer "POMODORO"))
                         (when drawer
                           (goto-char beg)
                           (let ((drawer-re (concat "^[ \t]*:" (regexp-quote drawer) ":[ \t]*$")))
                             (while (re-search-forward drawer-re end t)
                               (let ((element (org-element-at-point)))
                                 (when (eq (org-element-type element) 'drawer)
                                   (let ((cend (org-element-property :contents-end element)))
                                     (if (and (not org-log-states-order-reversed) cend)
                                         (goto-char cend)
                                       (forward-line))
                                     (throw 'exit t)))))))
                         (goto-char beg)
                         (let ((clock-re (concat "^[ \t]*" org-clock-string))
                               (count 0)
                               positions)
                           (save-excursion
                             (while (re-search-forward clock-re end t)
                               (let ((element (org-element-at-point)))
                                 (when (eq (org-element-type element) 'clock)
                                   (setq positions (cons (line-beginning-position) positions)
                                         count (1+ count))))))
                           (cond
                            ((null positions)
                             (org-end-of-meta-data)
                             (unless (bolp) (insert "\n")))
                            (drawer
                             (org-end-of-meta-data)
                             (let ((beg (point)))
                               (insert ":END:\n")
                               (let ((end (point-marker)))
                                 (goto-char beg)
                                 (save-excursion (insert ":" drawer ":\n"))
                                 (org-flag-region (line-end-position) (1- end) t 'outline)
                                 (org-indent-region (point) end)
                                 (forward-line)
                                 (unless org-log-states-order-reversed
                                   (goto-char end)
                                   (beginning-of-line -1))
                                 (set-marker end nil))))
                            (org-log-states-order-reversed (goto-char (car (last positions))))
                            (t (goto-char (car positions))))))
                       t)
                 (progn ,@body)))))))

(defun org-pomodoro-drawer-insert-item (item &optional content)
  (org-pomodoro-with-current-drawer
   (insert-before-markers "\n")
   (backward-char 1)
   (if (org-in-item-p) (org-insert-item) (progn (org-indent-line) (insert "- ")))
   (insert item ": ")
   (org-insert-time-stamp (current-time) t t)
   (unless (or (null content) (string-empty-p content))
     (insert " \\\\")
     (org-newline-and-indent)
     (insert content)
     (org-indent-line))))

(defvar org-pomodoro-interrupt-count 0)

(defun org-pomodoro-kill@before (&rest _)
  (setq org-pomodoro-interrupt-count 0)
  (call-interactively #'org-pomodoro-abort))

(advice-add #'org-pomodoro-kill :before #'org-pomodoro-kill@before)

(defun org-pomodoro-finished@before (&rest _)
  (setq org-pomodoro-interrupt-count 0)
  (org-pomodoro-drawer-insert-item "FINISHED"))

(advice-add #'org-pomodoro-finished :before #'org-pomodoro-finished@before)

(defun org-pomodoro-interrupt (cause)
  (interactive "sPlease enter the reason caused you to be interrupted: ")
  (setq org-pomodoro-interrupt-count (+ org-pomodoro-interrupt-count 1))
  (org-pomodoro-drawer-insert-item "INTERRUPTED" cause)
  (when (and (>= org-pomodoro-interrupt-count 3) (y-or-n-p "You've been interrupted more than 3 times. It's recommended to abort the pomodoro. Abort current pomodoro now ?"))
    (org-pomodoro-abort "Interrupted more than 3 times.")))

(defun org-pomodoro-abort (cause)
  (interactive "sPlease enter the reason that caused you to abort the pomodoro: ")
  (org-pomodoro-drawer-insert-item "KILLED" cause)
  (if (org-pomodoro-active-p) (org-pomodoro-killed))
  (setq org-pomodoro-interrupt-count 0))

(provide 'org-pomodoro-ext)
