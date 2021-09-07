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

(defun iso-week-to-time (year week day)
  (pcase-let ((`(,m ,d ,y)
               (calendar-gregorian-from-absolute
                (calendar-iso-to-absolute (list week day year)))))
    (encode-time 0 0 0 d m y)))
(defun iso-week-to-date (week day year)
  (calendar-gregorian-from-absolute
   (calendar-iso-to-absolute (list week day year))))
(defun iso-week-from-date (month day year)
  (calendar-iso-from-absolute
   (calendar-absolute-from-gregorian (list month day year))))

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
