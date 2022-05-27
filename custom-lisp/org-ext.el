;;;  -*- lexical-binding: t; -*-

(eval-when-compile (require 'subr-x)
                   (require 'cl-lib))
(require 'org)
(require 'org-agenda)

(defun org-subtree-content-at-point ()
  "Get the content text of the subtree at point and add it to the `kill-ring'.
Excludes the heading and any child subtrees."
  (interactive)
  (if (org-before-first-heading-p)
      (message "Not in or on an org heading")
    (save-excursion
      ;; If inside heading contents, move the point back to the heading
      ;; otherwise `org-agenda-get-some-entry-text' won't work.
      (unless (org-at-heading-p) (org-previous-visible-heading 1))
      (substring-no-properties
       (org-agenda-get-some-entry-text
        (point-marker)
        most-positive-fixnum)))))

(defun org-agenda-get-day-entries-with-macro-expanded (file date &rest args)
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
                         (org-dlet
                             ((date date))
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
		                (push deadlines results))))))))))
          (primitive-undo 1 buffer-undo-list)
          ret)))))

(with-eval-after-load 'org-agenda
  (if (version<= "9.5" (org-version))
      (advice-add #'org-agenda-get-day-entries :override #'org-agenda-get-day-entries-with-macro-expanded)
    (error "Org < 9.5 is not supported!")))

(defun org-get-media-link-export-function (media-type)
  (lambda (path _desc backend)
    (let ((_:ext (file-name-extension path)))
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

(defun org--deadline-or-schedule@after-scheduled (_arg type _time)
  (when (eq type 'scheduled) (org-todo (org-get-todo-sequence-head org-todo-heads))))

(advice-add #'org--deadline-or-schedule :after #'org--deadline-or-schedule@after-scheduled)

(defun org-agenda-review (time-start &optional time-end level)
  (let* ((buffer (get-buffer-create "*Org Agenda Review*"))
         (level (or level 3))
         (time-format "<%Y-%m-%d>")
         (time-start-string (format-time-string time-format time-start))
         (time-end-string (format-time-string time-format time-end))
         (time-end-inclusive-string (format-time-string time-format (encode-time (--map-indexed (pcase it-index
                                                                                                  (`3 (- it 1))
                                                                                                  (_ it))
                                                                                                (decode-time time-end))))))
    (pcase (get-buffer-window buffer)
      (`nil (switch-to-buffer-other-window buffer))
      (window (select-window window)))
    (read-only-mode -1)
    (erase-buffer)
    (unless (eq major-mode 'org-mode) (org-mode))
    (org-insert-heading)
    (insert "Org Agenda Review " (if (string-equal time-start-string time-end-inclusive-string) time-start-string (format "%s--%s" time-start-string time-end-inclusive-string)) "\n")
    (insert (format "#+BEGIN: clocktable :scope agenda :maxlevel %d :tstart \"%s\" :tend \"%s\"\n#+END:" level
                    time-start-string time-end-string))
    (org-ctrl-c-ctrl-c)
    (let ((beg (point)))
      (goto-char (point-min))
      (org-indent-region (point) beg))  ;有先后顺序
    (forward-line)
    (let ((beg (point)))
      (forward-line 2)
      (delete-region beg (point)))
    (goto-char (point-max))
    (let ((beg (point)))
      (forward-line -1)
      (end-of-line)
      (delete-region beg (point)))
    (goto-char (point-min))
    (read-only-mode +1)
    (let ((map (copy-keymap org-mode-map)))
      (define-key map (kbd "q") #'quit-window)
      (define-key map (kbd "d") #'org-agenda-daily-review)
      (define-key map (kbd "w") #'org-agenda-weekly-review)
      (define-key map (kbd "m") #'org-agenda-monthly-review)
      (define-key map (kbd "y") #'org-agenda-yearly-review)
      (define-key map (kbd "n") #'next-line)
      (define-key map (kbd "p") #'previous-line)
      (use-local-map map))))

(defun org-agenda-daily-review (&optional time)
  (interactive)
  (let ((time (or time (current-time))))
    (org-agenda-review time
                       (encode-time
                        (--map-indexed
                         (pcase it-index 
                           (`3 (+ it 1))
                           (_ it))
                         (decode-time time))))
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

(defun org-up-heading ()
  (interactive)
  (org-up-heading-or-point-min))

(defun org-down-heading ()
  (interactive)
  (org-goto-first-child))

(defvar org-mode-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'org-next-visible-heading)
    (define-key map (kbd "C-p") #'org-previous-visible-heading)
    (define-key map (kbd "C-f") #'org-forward-heading-same-level)
    (define-key map (kbd "C-b") #'org-backward-heading-same-level)
    (define-key map (kbd "C-u") #'org-up-heading)
    (define-key map (kbd "C-i") #'org-down-heading)
    (dolist (it '(org-next-visible-heading org-previous-visible-heading org-forward-heading-same-level org-backward-heading-same-level org-up-heading org-down-heading)) (put it 'repeat-map 'org-mode-navigation-repeat-map))
    map)
  "Keymap to repeat `org-mode' navigation key sequences.  Used in `repeat-mode'.")

(defvar org-mode-archive-subtree-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") #'org-archive-subtree-default)
    (put #'org-archive-subtree-default 'repeat-map 'org-mode-archive-subtree-repeat-map)
    map)
  "Keymap to repeat `org-mode' archive subtree key sequences.  Used in `repeat-mode'.")

(defun org-link-make-from-region (beg end &optional _desc)
  (interactive "*r\n")
  (let ((link (buffer-substring beg end)))
    (delete-region beg end)
    (insert "[[" link "][" link "]]")))

(provide 'org-ext)
;;; org-ext.el ends here
