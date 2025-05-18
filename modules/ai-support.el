;; -*- lexical-binding: t; -*-

(use-package gptel
  :when (member 'gptel extra-features)
  :ensure t
  :defer t
  :bind (("C-c a c" . gptel-add)
         ("C-c a C" . gptel-context-remove-all)
         ("C-c a r" . gptel-rewrite)
         ("C-c a a" . gptel))
  :custom
  (gptel-display-buffer-action '(display-buffer-same-window))
  (gptel-use-curl t)
  (gptel-expert-commands t))

(defmacro ai-support-define-tool (name categories arguments &rest options)
  (declare (doc-string 4) (indent defun))
  (cl-flet ((underscore (symbol)
              (replace-regexp-in-string (rx "-") "_" (symbol-name symbol)))
            (option (symbol)
              (car (alist-get symbol options))))
    (let ((function-name (intern (format "%s/%s" 'ai-support-tool name))))
      (when (member 'gptel extra-features)
        `(progn
           (defun ,function-name ,(mapcar #'cl-first arguments)
             ,@(cl-loop for argument in (mapcar #'cl-first arguments)
                        collect `(when (eq ,argument :json-false) (setf ,argument nil)))
             . ,(alist-get :progn options))
           (with-eval-after-load 'gptel
             (gptel-make-tool
              :name ,(underscore name)
              :function #',function-name
              :description ,(option :documentation)
              :args ',(cl-loop for (name . options) in arguments
                               collect `(:name ,(underscore name) :type ,(cl-getf options :type) :description ,(cl-getf options :documentation)))
              :category ,(symbol-name (cl-first categories))
              . ,(cl-loop for option in options
                          for (option-name nil) = option
                          unless (member option-name '(:documentation :progn))
                          append option))))))))

(ai-support-define-tool write-file (filesystem)
  ((file :type string :documentation "Path to the new file.")
   (content :type string :documentation "Content to write to the file."))
  (:documentation "Write content to a file, overwriting it if it already exists.")
  (:confirm t)
  (:progn
   (with-current-buffer (find-file-noselect file)
     (delete-region (point-min) (point-max))
     (insert content)
     (save-buffer))
   (format "File written successfully: %s" file)))

(ai-support-define-tool rename-file (filesystem)
  ((file :type string :documentation "Path to the file to rename.")
   (new-name :type string :documentation "New name for the file."))
  (:documentation "Rename a file to the specified new name.")
  (:confirm t)
  (:progn
   (if (not (file-exists-p file))
       (error "File does not exist: %s" file)
     (rename-file file new-name t)
     (format "File renamed successfully from %s to %s" file new-name))))

(ai-support-define-tool delete-file (filesystem)
  ((file :type string :documentation "Path to the file to delete."))
  (:documentation "Delete the specified file.")
  (:confirm t)
  (:progn
   (if (not (file-exists-p file))
       (error "File does not exist: %s" file)
     (delete-file file)
     (format "File deleted successfully: %s" file))))

(ai-support-define-tool list-directory (filesystem)
  ((directory :type string :documentation "Path to the directory to list.")
   (recursive :type boolean :documentation "Whether to list contents recursively."))
  (:documentation "List contents of a directory, optionally recursively. You may need to check this before most operations.")
  (:confirm t)
  (:include t)
  (:progn
   (if (not (file-directory-p directory))
       (error "Not a directory: %s" directory)
     (let* ((abs-directory (expand-file-name directory))
            (files (if recursive
                       (directory-files-recursively abs-directory ".*")
                     (directory-files abs-directory t)))
            (relative-files (mapcar (lambda (f) (file-relative-name f abs-directory)) files)))
       (if (null relative-files)
           "Directory is empty"
         (mapconcat (lambda (f) (format "%s" f)) relative-files "\n"))))))

(ai-support-define-tool read-file (filesystem)
  ((file :type string :documentation "Path to the file to read."))
  (:documentation "Read the contents of a file and return it as a string.")
  (:confirm nil)
  (:include t)
  (:progn
   (if (not (file-exists-p file))
       (error "File does not exist: %s" file)
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string)))))
  
(ai-support-define-tool replace-in-file (filesystem)
  ((file :type string :documentation "Path to the file to modify.")
   (from :type string :documentation "String to replace in the file.")
   (to :type string :documentation "String to replace with."))
  (:documentation "Replace all occurrences of a string in a file. You can utilize this to modify the existing code.")
  (:confirm t)
  (:progn
   (if (not (file-exists-p file))
       (error "File does not exist: %s" file)
     (with-current-buffer (find-file-noselect file)
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward (regexp-quote from) nil t)
           (replace-match to t t)))
       (save-buffer))
     (format "Replaced all occurrences of '%s' with '%s' in %s" from to file))))

(ai-support-define-tool shell-command (cmd)
  ((command :type string :documentation "Shell command to execute."))
  (:documentation "Execute shell command and return output.")
  (:confirm t)
  (:include t)
  (:progn
   (let ((result (shell-command-to-string command)))
     (if (string-empty-p result)
         "Command executed successfully (no output)"
       result))))

(provide 'ai-support)
;;; ai-support.el ends here
