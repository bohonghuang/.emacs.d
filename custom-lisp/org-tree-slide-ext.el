;; -*- lexical-binding: t; -*-

(require 'org-tree-slide)

(defun org-tree-slide--set-slide-header@around (fun &rest args)
  (if (booleanp org-tree-slide-header)
      (apply fun args)
    (defvar org-tree-slide-author)
    (defvar org-tree-slide-email)
    (defvar org-tree-slide-date)
    (defvar org-tree-slide-title)
    (defvar org-tree-slide-header)
    (let ((org-tree-slide-author nil)
          (org-tree-slide-email nil)
          (org-tree-slide-date nil)
          (org-tree-slide-title "")
          (org-tree-slide-header t))
      (apply fun args))))

(advice-add #'org-tree-slide--set-slide-header :around #'org-tree-slide--set-slide-header@around)

(defcustom org-tree-slide-line-spacing nil
  "Line spacing for `org-tree-slide-mode'.")
(defcustom org-tree-slide-text-scale nil
  "Text scale for `org-tree-slide-mode'.")
(defcustom org-tree-slide-minor-mode-alist nil
  "Specify what minor mode should be enabled or disabled for `org-tree-slide-mode'.")

(defvar org-tree-slide--original-line-spacing nil)
(defvar org-tree-slide--original-text-scale nil)
(defvar org-tree-slide--original-minor-mode-alist nil)

(defun org-tree-slide-ext-play-hook (&rest _)
  (when org-tree-slide-line-spacing
    (setq-local org-tree-slide--original-line-spacing (or line-spacing 0)
                line-spacing org-tree-slide-line-spacing))
  (when text-scale-mode-amount
    (setq-local org-tree-slide--original-text-scale text-scale-mode-amount)
    (text-scale-set org-tree-slide-text-scale))
  (setq-local org-tree-slide--original-minor-mode-alist nil)
  (pcase-dolist (`(,mode . ,arg) org-tree-slide-minor-mode-alist)
    (when (boundp mode)
      (push (cons mode (symbol-value mode)) org-tree-slide--original-minor-mode-alist)
      (funcall mode (pcase arg ('t +1) ('nil -1) (x x))))))

(defun org-tree-slide-ext-stop-hook (&rest _)
  (when org-tree-slide--original-line-spacing
    (setq-local line-spacing (pcase org-tree-slide--original-line-spacing (0 nil) (x x))
                org-tree-slide--original-line-spacing nil))
  (when org-tree-slide--original-text-scale
    (text-scale-set org-tree-slide--original-text-scale)
    (setq-local org-tree-slide--original-text-scale nil))
  (pcase-dolist (`(,mode . ,arg) org-tree-slide--original-minor-mode-alist)
    (funcall mode arg))
  (setq-local org-tree-slide--original-minor-mode-alist nil))

(add-hook 'org-tree-slide-play-hook #'org-tree-slide-ext-play-hook)
(add-hook 'org-tree-slide-stop-hook #'org-tree-slide-ext-stop-hook)

(provide 'org-tree-slide-ext)
