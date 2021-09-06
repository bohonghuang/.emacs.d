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
