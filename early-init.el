;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(when (string= system-type "android")
  (let* ((prefix "/data/data/com.termux/files/")
         (path (expand-file-name "usr/bin" prefix)))
    (setenv "PREFIX" prefix)
    (setenv "PATH" (format "%s:%s" (getenv "PATH") path))
    (setf exec-path (cl-delete-if-not #'file-readable-p exec-path)
          exec-path (nconc (butlast exec-path) (cons path (last exec-path))))))

(provide 'early-init)
;;; early-init.el ends here
