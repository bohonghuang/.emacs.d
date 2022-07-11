;; -*- lexical-binding: t; -*-
(cl-defstruct doom-modeline-pokemon
  number
  form)

(defcustom doom-modeline-pokemon-icon-path-elements '("icon" (format "%03d" number) (when form (format "_%d" (if (numberp form) form 1))) ".png")
  "Pokemon icon path elements concatenated by `doom-modeline-pokemon-icon-path'.")

(defun doom-modeline-pokemon-icon-path (pokemon)
  (let ((number (doom-modeline-pokemon-number pokemon))
        (form (doom-modeline-pokemon-form pokemon)))
    (defvar number nil)
    (defvar form nil)
    (apply #'concat (eval `(list . ,doom-modeline-pokemon-icon-path-elements)))))

(defcustom doom-modeline-pokemon-major-mode-alist
  '((fundamental-mode . 151)
    (org-mode . 647)
    (emacs-lisp-mode . 150)
    (inferior-emacs-lisp-mode . (:number 150 :form 1))
    (eshell-mode . (:number 150 :form 2))
    (dired-mode . 369)
    (Custom-mode . 601)
    (lisp-mode . 675)
    (yaml-mode . 717)
    (rust-mode . 99)
    (rustic-mode . 99)
    (emms-playlist-mode . 648)
    (calc-mode . 376)
    (calc-trail-mode . 376)
    (js-mode . 323)
    (java-mode . 324)
    (shell-mode . 366)
    (vterm-mode . 494)
    (scala-mode . 851)
    (python-mode . 195)
    (dired-mode . 369)
    (eww-mode . 655)
    (typescript-mode . 9)
    (json-mode . 544)
    (c-mode . 60)
    (objc-mode . 61)
    (c++-mode . 62)
    (html-mode . 322)
    (compilation-mode . 764)
    (backtrace-mode . 770)
    (help-mode . 479)
    (magit-status-mode . 65))
  "Pokemon definition alist for `major-mode'.")

(defcustom doom-modeline-pokemon-number 898
  "Max number of the available pokemon.")

(defun doom-modeline-pokemon-by-definition (def)
  (pcase def
    ((cl-type integer) (make-doom-modeline-pokemon :number def))
    ((cl-type list) (apply #'make-doom-modeline-pokemon def))
    ((cl-type doom-modeline-pokemon) def)
    (_ (error "Wrong pokemon definition: %s" def))))

(defun doom-modeline-pokemon-by-major-mode (&optional mode)
  (let* ((mode (or mode major-mode))
         (def (alist-get mode doom-modeline-pokemon-major-mode-alist)))
    (doom-modeline-pokemon-by-definition (or def (doom-modeline-pokemon-by-object (symbol-name mode))))))

(defun doom-modeline-pokemon-by-object (object)
  (make-doom-modeline-pokemon :number (1+ (mod (sxhash object) doom-modeline-pokemon-number))))

(defun doom-modeline-pokemon-icon (pokemon &optional text)
  (propertize (or text " ") 'display
              `((slice ,(* (% (truncate(/ (float-time) 0.15)) 2) 0.5) 0.0 0.5 1.0)
                (image :type png :file ,(doom-modeline-pokemon-icon-path pokemon) :ascent center))))

(defun doom-modeline-pokemon-buffer-mode-icon ()
  (doom-modeline-pokemon-icon (doom-modeline-pokemon-by-major-mode)))

(defun doom-modeline-pokemon-buffer-name-icon ()
  (doom-modeline-pokemon-icon (doom-modeline-pokemon-by-object (buffer-name))))

(doom-modeline-def-segment buffer-info-pokemon
  "Combined information about the current buffer.
      
Including the current working directory, the file name, and its state (modified,
read-only or non-existent)."
  (concat
   (doom-modeline-pokemon-buffer-mode-icon)
   doom-modeline-spc
   (doom-modeline--buffer-state-icon)
   (doom-modeline--buffer-name)
   doom-modeline-spc
   (doom-modeline-pokemon-buffer-name-icon)))

(doom-modeline-def-segment buffer-default-directory-pokemon
  "Displays `default-directory' with the icon and state.
        
This is for special buffers like the scratch buffer where knowing the current
project directory is important."
  (let ((face (doom-modeline-face
               (if (and buffer-file-name (buffer-modified-p))
                   'doom-modeline-buffer-modified
                 'doom-modeline-buffer-path))))
    (concat (and doom-modeline-major-mode-icon
                 (concat (doom-modeline-pokemon-buffer-mode-icon)
                         doom-modeline-vspc))
            (doom-modeline--buffer-state-icon)
            (propertize (abbreviate-file-name default-directory) 'face face))))

(define-minor-mode doom-modeline-pokemon-mode
  "Minor mode to put Pokemon icons on doom modeline."
  :global t
  (if doom-modeline-pokemon-mode
      (progn
        (advice-add #'doom-modeline-segment--buffer-info :override #'doom-modeline-segment--buffer-info-pokemon)
        (advice-add #'doom-modeline-segment--buffer-default-directory :override #'doom-modeline-segment--buffer-default-directory-pokemon))
    (advice-remove #'doom-modeline-segment--buffer-info #'doom-modeline-segment--buffer-info-pokemon)
    (advice-remove #'doom-modeline-segment--buffer-default-directory #'doom-modeline-segment--buffer-default-directory-pokemon)))

(provide 'doom-modeline-pokemon)
