(require 'cl-lib)

(defun emacs-lisp-macroexpand-buffer-undo ()
  (interactive)
  (let ((buffer-read-only nil))
    (undo)))

(defun emacs-lisp-macroexpand-to-buffer (function)
  (cl-flet ((sexp-range () (list (point) (save-excursion (forward-sexp) (point)))))
    (let ((buffer-name "*emacs-lisp-macroexpand*"))
      (cl-destructuring-bind (beg end) (sexp-range)
        (let ((exp-string (buffer-substring beg end)))
          (if (not (string= (buffer-name (current-buffer)) buffer-name))
              (let ((buffer (get-buffer-create buffer-name)))
                (with-current-buffer buffer
                  (let ((buffer-read-only nil))
                    (emacs-lisp-mode)
                    (delete-region (point-min) (point-max))
                    (pp (save-window-excursion
                          (funcall function (car (read-from-string exp-string))))
                        (current-buffer))
                    (indent-region (point-min) (point-max))
                    (goto-char (point-min))
                    (let ((map (make-sparse-keymap)))
                      (define-key map (kbd "q") #'quit-window)
                      (define-key map (kbd "a") #'emacs-lisp-macroexpand-all)
                      (define-key map (kbd "C-c RET") #'emacs-lisp-macroexpand)
                      (define-key map (kbd "C-/") #'emacs-lisp-macroexpand-buffer-undo)
                      (use-local-map map)))
                  (read-only-mode +1))
                (pulse-momentary-highlight-region beg end)
                (pop-to-buffer buffer))
            (let ((buffer-read-only nil))
              (save-excursion
                (delete-region beg end)
                (pp (funcall function (car (read-from-string exp-string))) (current-buffer))
                (when (looking-back (rx bol) (point-min))
                  (delete-char -1))
                (indent-region beg (point)))
              (apply #'pulse-momentary-highlight-region (sexp-range)))))))))

(defun emacs-lisp-macroexpand@around (fun &rest args)
  (if (called-interactively-p nil)
      (emacs-lisp-macroexpand-to-buffer #'macroexpand-1)
    (apply fun args)))

(advice-add #'emacs-lisp-macroexpand :around #'emacs-lisp-macroexpand@around)

(defun emacs-lisp-macroexpand-all ()
  (interactive)
  (emacs-lisp-macroexpand-to-buffer #'macroexpand-all))

(defvar disassemble-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map))

(defun disassemble@after (&rest args)
  (current-local-map)
  (with-current-buffer (get-buffer "*Disassemble*")
    (when (eq major-mode 'asm-mode)
      (unless (eql (current-local-map) disassemble-buffer-map)
        (use-local-map disassemble-buffer-map)))))

(advice-add #'disassemble :after #'disassemble@after)

(provide 'elisp-mode-ext)
