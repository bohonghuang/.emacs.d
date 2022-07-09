;;; -*- lexical-binding: t -*-

(require 'smartparens)
(require 'drag-stuff)

(defun intellij-edit-line-blank-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:blank:]]*$")))

(defun intellij-edit-line-indentation ()
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (back-to-indentation)
      (- (point) beg))))

(defun intellij-edit-cc-backspace (arg)
  (interactive "*P")
  (if (or (region-active-p) (not (looking-back "^[[:blank:]]*" (line-beginning-position))))
      (backward-delete-char-untabify (prefix-numeric-value arg))
    (let* ((beg (point))
           (end (progn (indent-for-tab-command) (point))))
      (when (<= beg end)
        (if (save-excursion (forward-line -1) (intellij-edit-line-blank-p))
            (progn (delete-region (line-beginning-position 0) (line-beginning-position)) (back-to-indentation))
          (delete-indentation))))))

(defun intellij-edit-pycharm-return (&optional arg)
  (interactive "*P")
  (if (looking-back "^[[:blank:]]*" (line-beginning-position))
    (let ((arg (or arg 1))
          (indent (buffer-substring-no-properties (line-beginning-position) (point))))
      (dotimes (_ arg)
        ;; (delete-region (line-beginning-position) (point))
        (newline)
        (insert indent)))
    (call-interactively #'newline arg)))

(defun intellij-edit-pycharm-backspace (&optional arg)
  (interactive "*P")
  (if (region-active-p)
      (delete-region (region-beginning) (region-end))
    (python-indent-dedent-line-backspace (or arg 1))))

(defun intellij-edit-drag-stuff-after-drag ()
  (if (region-active-p)
      (progn
        (indent-region-line-by-line (region-beginning) (region-end))
        (setq deactivate-mark nil))
    (indent-for-tab-command)))

(add-hook 'drag-stuff-after-drag-hook #'intellij-edit-drag-stuff-after-drag)

(defvar intellij-edit-python-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") #'intellij-edit-pycharm-backspace)
    (define-key map (kbd "RET") #'intellij-edit-pycharm-return)
    map)
  "Keymap to provide Intellij style editing. Used in `intellij-edit-indent-mode'.")

(defvar intellij-edit-cc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") #'intellij-edit-cc-backspace)
    map)
  "Keymap to provide Intellij style editing. Used in `intellij-edit-cc-mode'.")

(define-minor-mode intellij-edit-python-mode
  "Minor mode to provide Intellij style editing."
  :keymap intellij-edit-python-mode-map)

(define-minor-mode intellij-edit-cc-mode
  "Minor mode to provide Intellij style editing."
  :keymap intellij-edit-cc-mode-map)

(provide 'intellij-edit)
