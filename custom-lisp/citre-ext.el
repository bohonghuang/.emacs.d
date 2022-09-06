(require 'citre)
(require 'citre-global)

(defcustom citre-gtags-label "native"
  "Specify GTAGSLABEL for running `gtags' or `global'.")

(defun citre-global-auto-objdir ()
  (kill-local-variable 'citre-gtags-args)
  (setq-local citre-gtags-args (append citre-gtags-args
                                       (list "--gtagslabel" citre-gtags-label)
                                       (list "--objdir" (file-relative-name (file-name-directory citre--tags-file) (project-root (project-current t)))))))

(defun citre-global-auto-objdir-wrapper (fun &rest args)
  (when-let* ((file (buffer-file-name (current-buffer)))
              (project (project-current t))
              (root (project-root project))
              (ctags-file citre--tags-file)
              (gtags-directory (file-name-directory ctags-file)))
    (let ((process-environment (append (list (concat "GTAGSOBJDIR=" "./" (file-relative-name gtags-directory root))
                                             (concat "GTAGSLABEL=" citre-gtags-label))
                                       process-environment)))
      (apply fun args))))

(defun citre-global-update-database-this-file ()
  (interactive)
  (apply #'citre-global-auto-objdir-wrapper
         (list (lambda ()
                 (citre-global--get-output-lines (list "--single-update" (file-relative-name (buffer-file-name (current-buffer)) default-directory)))))))

(defun citre-global--get-reference-lines-using-relative-path (name &optional case-fold start-file)
  (let* ((name (when name (substring-no-properties name)))
         inhibit-message
         cmd)
    (when case-fold (push "--ignore-case" cmd))
    (push (or citre-global-program "global") cmd)
    ;; Global doesn't know how to expand "~", so we need to expand START-FILE.
    (when start-file (push (concat "--nearness=" (file-relative-name (expand-file-name start-file) default-directory))
                           cmd))
    (setq cmd (append (nreverse cmd) citre-global--find-references-args
                      (list "--" name)))
    (citre-get-output-lines cmd)))

(defun citre-auto-update-tags-after-save ()
  (when citre-mode
    (when citre--tags-file
      (citre-update-this-tags-file))
    (citre-global-update-database-this-file)))

(defun citre-init-in-project ()
  (interactive)
  (let ((citre-default-create-tags-file-location 'project-cache)
        (citre-use-project-root-when-creating-tags t))
    (citre-create-tags-file)))

(advice-add #'citre-jump-to-reference :around #'citre-global-auto-objdir-wrapper)
(advice-add #'citre-xref--global-find-reference :around #'citre-global-auto-objdir-wrapper)
(advice-add #'citre-global-update-database :around #'citre-global-auto-objdir-wrapper)
(advice-add #'citre-peek-references :around #'citre-global-auto-objdir-wrapper)
(advice-add #'citre-global--get-reference-lines :override #'citre-global--get-reference-lines-using-relative-path)

(provide 'citre-ext)
