;;;  -*- lexical-binding: t; -*-
(eval-when-compile (require 'subr-x)
                   (require 'cl-lib))
(require 'org)

(defun org-yank-fdef-cpp()
  (interactive)
  (let* ((str (replace-regexp-in-string " *= *\\w+" "" (replace-regexp-in-string "[\n<>:&\\*]" "" (substring-no-properties (car kill-ring)))))
         (name-args (when (string-match " *\\w+ +\\(\\w+\\) *(\\(.+\\)) *;\\{0,1\\}" str)
                      (list (match-string 0 str)
                            (match-string 1 str)
                            (match-string 2 str))))
         (name (nth 1 name-args))
         (args (nth 2 name-args))
         (args (split-string args "," nil "[ \n]"))
         (args (cl-loop for arg in args collect
                        (when (string-match "\\w+ +\\(\\w+\\)" arg)
                          (nth 1 (list (match-string 0 arg) (match-string 1 arg)))
                          )))
         (args (seq-filter (lambda (arg) arg) args)))
    (org-insert-heading-respect-content)
    (insert (concat "~" name "~"))
    (org-return t)
    (insert "- 参数：")
    ;; (insert (concat "- ~" name "~ ：")) as list
    (let ((first-loop t))
      (cl-loop for arg in args do
               ;; (if first-loop
               ;;     (progn (org-return t)
               ;;     (insert "- ")
               (org-meta-return)
               (if first-loop (org-metaright))
               (insert (concat "~" arg "~ ："))
               (setq first-loop nil)
               ))))

(defun org-subtree-content-at-point ()
  "Get the content text of the subtree at point and add it to the `kill-ring'.
Excludes the heading and any child subtrees."
  (interactive)
  (if (org-before-first-heading-p)
      (message "Not in or on an org heading")
    (save-excursion
      ;; If inside heading contents, move the point back to the heading
      ;; otherwise `org-agenda-get-some-entry-text' won't work.
      (unless (org-on-heading-p) (org-previous-visible-heading 1))
      (substring-no-properties
       (org-agenda-get-some-entry-text
        (point-marker)
        most-positive-fixnum)))))

(require 'cal-iso)

(defun iso-week-to-time (iso-week)
  (pcase-let ((`(,m ,d ,y)
               (calendar-gregorian-from-absolute
                (calendar-iso-to-absolute iso-week))))
    (encode-time 0 0 0 d m y)))

(defun iso-week-to-date (iso-week)      ;week day year
  (calendar-gregorian-from-absolute
   (calendar-iso-to-absolute iso-week)))

(defun iso-week-from-date (date)
  (calendar-iso-from-absolute
   (calendar-absolute-from-gregorian date)))

(defun time-to-date (time)
  (let ((decoded-time (decode-time time)))
    (list (nth 4 decoded-time) (nth 3 decoded-time) (nth 5 decoded-time))))

(defun org-agenda-get-day-entries (file date &rest args)
  "Does the work for `org-diary' and `org-agenda'.
FILE is the path to a file to be checked for entries.  DATE is date like
the one returned by `calendar-current-date'.  ARGS are symbols indicating
which kind of entries should be extracted.  For details about these, see
the documentation of `org-diary'."
  (let* ((org-startup-folded nil)
         (org-startup-align-all-tables nil)
         (buffer (if (file-exists-p file) (org-get-agenda-file-buffer file)
                   (error "No such file %s" file))))
    (if (not buffer)
        ;; If file does not exist, signal it in diary nonetheless.
        (list (format "ORG-AGENDA-ERROR: No such org-file %s" file))
      (with-current-buffer buffer
        (unless (derived-mode-p 'org-mode)
          (error "Agenda file %s is not in Org mode" file))
        (setq org-agenda-buffer (or org-agenda-buffer buffer))
        (setf org-agenda-current-date date)
        (undo-boundary)
        (org-macro-replace-all org-macro-templates)
        (let ((ret (save-excursion
                     (save-restriction
                       (if (eq buffer org-agenda-restrict)
                           (narrow-to-region org-agenda-restrict-begin
                                             org-agenda-restrict-end)
                         (widen))
                       ;; Rationalize ARGS.  Also make sure `:deadline' comes
                       ;; first in order to populate DEADLINES before passing it.
                       ;;
                       ;; We use `delq' since `org-uniquify' duplicates ARGS,
                       ;; guarding us from modifying `org-agenda-entry-types'.
                       (setf args (org-uniquify (or args org-agenda-entry-types)))
                       (when (and (memq :scheduled args) (memq :scheduled* args))
                         (setf args (delq :scheduled* args)))
                       (cond
                        ((memq :deadline args)
                         (setf args (cons :deadline
                                          (delq :deadline (delq :deadline* args)))))
                        ((memq :deadline* args)
                         (setf args (cons :deadline* (delq :deadline* args)))))
                       ;; Collect list of headlines.  Return them flattened.
                       (let ((case-fold-search nil) results deadlines)
                         (dolist (arg args (apply #'nconc (nreverse results)))
                           (pcase arg
                             ((and :todo (guard (org-agenda-today-p date)))
                              (push (org-agenda-get-todos) results))
                             (:timestamp
                              (push (org-agenda-get-blocks) results)
                              (push (org-agenda-get-timestamps deadlines) results))
                             (:sexp
                              (push (org-agenda-get-sexps) results))
                             (:scheduled
                              (push (org-agenda-get-scheduled deadlines) results))
                             (:scheduled*
                              (push (org-agenda-get-scheduled deadlines t) results))
                             (:closed
                              (push (org-agenda-get-progress) results))
                             (:deadline
                              (setf deadlines (org-agenda-get-deadlines))
                              (push deadlines results))
                             (:deadline*
                              (setf deadlines (org-agenda-get-deadlines t))
                              (push deadlines results)))))))))
          (primitive-undo 1 buffer-undo-list)
          ret)))))

(defun org-get-media-link-export-function (media-type)
  (lambda (path desc backend)
    (let ((ext (file-name-extension path)))
      (cond
       ((eq 'html backend)
        (format "<%s src='%s' controls/>" media-type (url-encode-url (replace-regexp-in-string "~" (expand-file-name "~") path))))
       (t nil)))))

(defconst org-media-file-extensions '(("video" . ("mkv" "mp4" "flv"))
                                      ("audio" . ("mp3" "aac" "m4a" "flac" "ogg"))))

(defun org-media-file-link-export (path desc backend)
  (cond
   ((eq 'html backend)
    (let* ((ext (file-name-extension path))
           (media-type-cons (--find (-contains-p (cdr it) ext) org-media-file-extensions)))
      (pcase media-type-cons
        (`nil nil)
        ((let media-type (car media-type-cons))
         (funcall (org-link-get-parameter media-type :export) path desc backend)))))
   (t nil)))

(org-link-set-parameters "video" :export (org-get-media-link-export-function "video"))
(org-link-set-parameters "audio" :export (org-get-media-link-export-function "audio"))
(org-link-set-parameters "file" :export #'org-media-file-link-export)

(defadvice org--deadline-or-schedule (after org--deadline-or-schedule-reset-todo-status)
  (org-todo (org-get-todo-sequence-head org-todo-heads)))

(ad-activate #'org--deadline-or-schedule)

(defun org-agenda-review (time-start &optional time-end level)
  (let* ((buffer (get-buffer-create "*Org Agenda Review*"))
         (level (and level 3))
         (time-format "<%Y-%m-%d>")
         (time-start-string (format-time-string time-format time-start))
         (time-end-string (format-time-string time-format time-end))
         (time-end-inclusive-string (format-time-string time-format (encode-time (--map-indexed (pcase it-index
                                                                                                  (`3 (- it 1))
                                                                                                  (_ it)) (decode-time time-end))))))
    (pcase (get-buffer-window buffer)
      (`nil (switch-to-buffer-other-window buffer))
      (window (select-window window)))
    (read-only-mode -1)
    (erase-buffer)
    (unless (eq major-mode 'org-mode) (org-mode))
    (org-insert-heading)
    (insert "Org Agenda Review " (if (string-equal time-start-string time-end-inclusive-string) time-start-string (format "%s--%s" time-start-string time-end-inclusive-string)) "\n")
    (insert (format "#+BEGIN: clocktable :scope agenda :maxlevel %d :tstart \"%s\" :tend \"%s\"\n#+END:" 2
                    time-start-string time-end-string))
    (org-ctrl-c-ctrl-c)
    (let ((beg (point)))
      (beginning-of-buffer)
      (org-indent-region (point) beg))  ;有先后顺序
    (next-line)
    (let ((beg (point)))
      (next-line 2)
      (delete-region beg (point)))
    (end-of-buffer)
    (let ((beg (point)))
      (previous-line)
      (end-of-line)
      (delete-region beg (point)))
    (beginning-of-buffer)
    (read-only-mode +1)
    (local-set-key (kbd "q") #'quit-window)
    (local-set-key (kbd "d") #'org-agenda-daily-review)
    (local-set-key (kbd "w") #'org-agenda-weekly-review)
    (local-set-key (kbd "m") #'org-agenda-monthly-review)
    (local-set-key (kbd "y") #'org-agenda-yearly-review)))

(defun org-agenda-daily-review (&optional time)
  (interactive)
  (let ((time (or time (current-time))))
    (org-agenda-review time
                     (encode-time
                      (--map-indexed
                       (pcase it-index 
                         (`3 (+ it 1))
                         (_ it)) (decode-time time))))
    (local-set-key (kbd "f") (lambda () (interactive) (org-agenda-daily-review
                                       (encode-time
                                        (--map-indexed (pcase it-index
                                                         (`3 (+ it 1))
                                                         (_ it))
                                                       (decode-time time))))))
    (local-set-key (kbd "b") (lambda () (interactive) (org-agenda-daily-review
                                       (encode-time
                                        (--map-indexed (pcase it-index
                                                         (`3 (- it 1))
                                                         (_ it))
                                                       (decode-time time))))))))

(defun org-agenda-weekly-review (&optional time)
  (interactive)
  (let* ((time (or time (current-time)))
         (iso-week (iso-week-from-date (time-to-date time)))
         (iso-this-week-start (--map-indexed (pcase it-index
                                               (`1 1)
                                               (_ it)) iso-week))
         (iso-next-week-start (--map-indexed (pcase it-index
                                               (`0 (+ it 1))
                                               (_ it)) iso-this-week-start)))
    (org-agenda-review (iso-week-to-time iso-this-week-start) (iso-week-to-time iso-next-week-start))
    (local-set-key (kbd "f") (lambda () (interactive) (org-agenda-weekly-review
                                       (encode-time
                                        (--map-indexed (pcase it-index
                                                         (`3 (+ it 7))
                                                         (_ it))
                                                       (decode-time time))))))
    (local-set-key (kbd "b") (lambda () (interactive) (org-agenda-weekly-review
                                       (encode-time
                                        (--map-indexed (pcase it-index
                                                         (`3 (- it 7))
                                                         (_ it))
                                                       (decode-time time))))))))

(defun org-agenda-monthly-review (&optional time)
  (interactive)
  (let ((time (or time (current-time))))
    (org-agenda-review (encode-time
                      (--map-indexed
                       (pcase it-index 
                         (`3 1)
                         (_ it)) (decode-time time)))
                     (encode-time
                      (--map-indexed
                       (pcase it-index 
                         (`3 1)
                         (`4 (+ it 1))
                         (_ it)) (decode-time time))))
    (local-set-key (kbd "f") (lambda () (interactive) (org-agenda-monthly-review
                                       (encode-time
                                        (--map-indexed (pcase it-index
                                                         (`3 1)
                                                         (`4 (+ it 1))
                                                         (_ it))
                                                       (decode-time time))))))
    (local-set-key (kbd "b") (lambda () (interactive) (org-agenda-monthly-review
                                       (encode-time
                                        (--map-indexed (pcase it-index
                                                         (`3 1)
                                                         (`4 (- it 1))
                                                         (_ it))
                                                       (decode-time time))))))))

(defun org-agenda-yearly-review (&optional time)
  (interactive)
  (let ((time (or time (current-time))))
    (org-agenda-review (encode-time
                        (--map-indexed
                         (pcase it-index 
                           ((or `3 4) 1)
                           (_ it)) (decode-time time)))
                       (encode-time
                        (--map-indexed
                         (pcase it-index 
                           ((or `3 `4) 1)
                           (`5 (+ it 1))
                           (_ it)) (decode-time time))))
    (local-set-key (kbd "f") (lambda () (interactive) (org-agenda-yearly-review
                                       (encode-time
                                        (--map-indexed (pcase it-index
                                                         ((or `3 `4) 1)
                                                         (`5 (+ it 1))
                                                         (_ it))
                                                       (decode-time time))))))
    (local-set-key (kbd "b") (lambda () (interactive) (org-agenda-yearly-review
                                       (encode-time
                                        (--map-indexed (pcase it-index
                                                         ((or `3 `4) 1)
                                                         (`5 (- it 1))
                                                         (_ it))
                                                       (decode-time time))))))))

(global-set-key (kbd "C-c g r") #'org-agenda-daily-review)

(provide 'org-ext)
