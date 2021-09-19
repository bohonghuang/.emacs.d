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

(defun org-pomodoro-into-drawer ()
  (let ((p (org-entry-get nil "CLOCK_INTO_DRAWER" 'inherit t)))
    (cond ((equal p "nil") nil)
	  ((equal p "t") "POMODORO")
          ((org-string-nw-p p)
	   (if (string-match-p "\\`[0-9]+\\'" p) (string-to-number p) p))
	  ((org-string-nw-p org-clock-into-drawer))
	  ((integerp org-clock-into-drawer) org-clock-into-drawer)
	  ((not org-clock-into-drawer) nil)
	  ((org-log-into-drawer))
	  (t "POMODORO"))))

(defmacro org-pomodoro-with-current-drawer (&rest body)
  (save-excursion
    `(with-current-buffer (marker-buffer org-clock-marker)
      (catch 'exit
         (goto-char org-clock-marker)
         (org-back-to-heading t)
         (let* ((beg (line-beginning-position))
	        (end (save-excursion (outline-next-heading) (point)))
	        (org-clock-into-drawer (org-pomodoro-into-drawer))
	        (drawer "POMODORO"))
           ;; Look for an existing clock drawer.
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
	     ;; Count the CLOCK lines and store their positions.
	     (save-excursion
	       (while (re-search-forward clock-re end t)
	         (let ((element (org-element-at-point)))
	           (when (eq (org-element-type element) 'clock)
		     (setq positions (cons (line-beginning-position) positions)
		           count (1+ count))))))
	     (cond
	      ((null positions)
	       ;; Skip planning line and property drawer, if any.
	       (org-end-of-meta-data)
	       (unless (bolp) (insert "\n"))
	       ;; Create a new drawer if necessary.
	       (when (and org-clock-into-drawer
		          (or (not (wholenump org-clock-into-drawer))
			      (< org-clock-into-drawer 2)))
	         (let ((beg (point)))
                   (insert ":" drawer ":\n:END:\n")
	           (org-indent-region beg (point))
	           (org-flag-region
	            (line-end-position -1) (1- (point)) t 'outline)
	           (forward-line -1))))
	      ;; When a clock drawer needs to be created because of the
	      ;; number of clock items or simply if it is missing, collect
	      ;; all clocks in the section and wrap them within the drawer.
	      ((if (wholenump org-clock-into-drawer)
	           (>= (1+ count) org-clock-into-drawer)
	         drawer)
	       ;; Skip planning line and property drawer, if any.
	       (org-end-of-meta-data)
	       (let ((beg (point)))
	         (insert
	          (mapconcat
	           (lambda (p)
		     (save-excursion
		       (goto-char p)
		       (org-trim (delete-and-extract-region
			          (save-excursion (skip-chars-backward " \r\t\n")
					          (line-beginning-position 2))
			          (line-beginning-position 2)))))
	           positions "\n")
	          "\n:END:\n")
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
	      (t (goto-char (car positions)))))))
      (progn ,@body))))

(defun org-pomodoro-interrupt (cause)
  (interactive "sEnter the reason caused you to be interrupted: ")
  (org-pomodoro-with-current-drawer
   (insert-before-markers "\n")
   (backward-char 1)
   (if (org-in-item-p) (org-insert-item) (progn (org-indent-line) (insert "- ")))
   (insert "INTERRUPTED: ")
   (org-insert-time-stamp (current-time) t t)
   (unless (string-empty-p cause)
     (insert " \\\\")
     (org-newline-and-indent)
     (insert cause)
     (org-indent-line))))
