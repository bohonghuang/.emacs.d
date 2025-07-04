;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This is an Emacs configuration file, in which modules are loaded lazily by `use-package'.

;;; Code:

(require 'cl-lib)
(require 'rx)

(let ((original-gc-cons-threshold gc-cons-threshold))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold original-gc-cons-threshold))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file) (load-file custom-file))

(cond ((< 25 emacs-major-version)
       (require 'package)
       (dolist (archive '(("melpa" . "https://melpa.org/packages/")
                          ("gnu-devel" . "https://elpa.gnu.org/devel/")
                          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
         (push archive package-archives)))
      ((< 26 emacs-major-version)
       (package-initialize)))

(unless (or (<= 29 emacs-major-version) (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package use-package
  :defer nil
  :ensure nil
  :custom
  (use-package-verbose t)
  (use-package-minimum-reported-time 0))

(use-package use-package-ensure
  :defer nil
  :ensure nil
  :config
  (defconst use-package-ensure-keywords '(:pin :ensure))
  (setq use-package-keywords
        (let ((use-package-keywords (cl-delete-if (lambda (x) (member x use-package-ensure-keywords))
                                                  use-package-keywords)))
          (let* ((pos (cl-position :unless use-package-keywords))
                 (head (cl-subseq use-package-keywords 0 (+ 1 pos)))
                 (tail (nthcdr (+ 1 pos) use-package-keywords)))
            (append head use-package-ensure-keywords tail)))))

(use-package quelpa-use-package
  :demand t
  :ensure t
  :init (setq quelpa-update-melpa-p nil
              quelpa-use-package-inhibit-loading-quelpa t))

;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in Packages ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :defer nil
  :ensure nil
  :config (set-language-environment "UTF-8"))

(use-package emacs-ext
  :load-path "custom-lisp"
  :demand t)

(use-package elisp-mode
  :defer t
  :ensure nil)

(use-package disass
  :ensure nil
  :defer t
  :bind (:map emacs-lisp-mode-map
         ("C-c M-d" . disassemble)))

(use-package elisp-mode-ext
  :load-path "custom-lisp"
  :demand t
  :after elisp-mode)

(use-package minibuffer
  :ensure nil
  :defer nil
  :custom
  (history-length 1000)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (enable-recursive-minibuffers t)
  :config
  (minibuffer-depth-indicate-mode +1))

(use-package package
  :ensure nil
  :defer nil)

(use-package comp
  :ensure nil
  :defer nil
  :custom
  (package-native-compile t)
  (native-comp-async-report-warnings-errors nil))

(use-package startup
  :ensure nil
  :defer nil
  :init (provide 'startup)
  :custom
  (inhibit-startup-screen t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message ""))

(use-package simple
  :ensure nil
  :defer nil
  :hook (prog-mode . toggle-word-wrap)
  :bind (("C-?" . undo-redo)
         ("S-DEL" . delete-indentation))
  :custom
  (column-number-mode t)
  (visible-bell t)
  (show-paren-mode nil)
  (indent-tabs-mode nil)
  (auto-hscroll-mode t)
  (word-wrap-by-category t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (put 'narrow-to-region 'disabled nil))

(use-package simple-ext
  :load-path "custom-lisp"
  :defer t
  :bind (("M-C" . filter-and-upcase-initials-region)
         ("M-SPC" . cycle-spacing-dwim)))

(use-package subr
  :ensure nil
  :defer t
  :init (defalias 'yes-or-no-p 'y-or-n-p)
  (defun adjust-frame-alpha-background (offset)
    (set-frame-parameter (selected-frame) 'alpha-background (max 0 (min (+ (frame-parameter (selected-frame) 'alpha-background) offset) 100))))
  (global-set-key (kbd "C-x 5 +") (defun increase-frame-alpha-background ()
                                    (interactive)
                                    (adjust-frame-alpha-background +10)))
  (global-set-key (kbd "C-x 5 -") (defun decrease-frame-alpha-background ()
                                    (interactive)
                                    (adjust-frame-alpha-background -10)))
  (global-set-key (kbd "C-x 5 =") (defun toggle-frame-undecorated ()
                                    (interactive)
                                    (cl-callf not (frame-parameter (selected-frame) 'undecorated))))
  (when (<= 28 emacs-major-version)
    (defvar frame-alpha-background-adjust-repeat-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "+") #'increase-frame-alpha-background)
        (define-key map (kbd "=") #'toggle-frame-undecorated)
        (define-key map (kbd "-") #'decrease-frame-alpha-background)
        (dolist (it '(increase-frame-alpha-background decrease-frame-alpha-background toggle-frame-undecorated))
          (put it 'repeat-map 'frame-alpha-background-adjust-repeat-map))
        map)
      "Keymap to repeat adjustment for the background alpha of the selected window. Used in `repeat-mode'.")))

(use-package files
  :ensure nil
  :defer t
  :custom
  (make-backup-files nil)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)
  :hook (find-file . (lambda ()
                       (unless (file-exists-p (file-truename buffer-file-name))
                         (set-buffer-file-coding-system 'utf-8))))
  :bind (("C-x C-w" . write-to-file-dwim))
  :config
  (defun write-to-file-dwim ()
    (interactive)
    (if (region-active-p)
        (call-interactively #'write-region)
      (call-interactively #'write-file))))

(use-package url-handlers
  :ensure nil
  :defer nil
  :config (url-handler-mode +1))

(defcustom extra-features nil
  "Extra features enabled on Emacs startup.")

(setq local-file (expand-file-name "local.el" user-emacs-directory))
(when (file-exists-p local-file) (load-file local-file))

(use-package window
  :ensure nil
  :defer nil
  :init (defalias 'window-buffer-change-hook 'window-buffer-change-functions)
  :config
  (defun previous-other-window ()
    (interactive)
    (other-window -1))
  (global-set-key (kbd "C-x O") #'previous-other-window)
  (when (<= 28 emacs-major-version)
    (define-key other-window-repeat-map (kbd "O") #'previous-other-window)
    (put #'previous-other-window 'repeat-map 'other-window-repeat-map))
  (defvar buffer-switch-repeat-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<left>") #'previous-buffer)
        (define-key map (kbd "<right>") #'next-buffer)
        (dolist (it '(previous-buffer next-buffer))
          (put it 'repeat-map 'buffer-switch-repeat-map))
        map)
      "Keymap to repeat window buffer navigation key sequences.  Used in `repeat-mode'."))

(use-package password-cache
  :ensure nil
  :defer t
  :custom
  (password-cache-expiry (* 5 60)))

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

(use-package term/xterm
  :when (not (display-graphic-p))
  :ensure nil
  :defer nil
  :custom (xterm-set-window-title t)
  :config
  (xterm-mouse-mode +1)
  (use-package term/xterm
    :when (string-equal (getenv "TERM") "fbterm")
    :ensure nil
    :config
    (tty-no-underline)
    (xterm-register-default-colors xterm-standard-colors)
    (use-package t-mouse
      :ensure nil
      :demand t
      :config
      (ignore-errors (gpm-mouse-enable)))))

(use-package xclip
  :when (and (not (display-graphic-p)) (executable-find "xclip"))
  :defer nil
  :ensure t
  :config
  (xclip-mode +1))

(use-package monokai-theme
  :when (null custom-enabled-themes)
  :ensure t
  :demand t
  :config
  (load-theme 'monokai t))

;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Packages ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package hl-line
  :ensure nil
  :defer nil
  :hook (prog-mode . hl-line-mode))

(use-package mode-fontify
  :quelpa (mode-fontify :fetcher github :repo "bohonghuang/mode-fontify")
  :defer t
  :commands (mode-fontify-region
             mode-fontify-face-region
             mode-fontify-face-region-at-point
             font-lock-update-dwim)
  :bind (("C-x x f" . font-lock-update-dwim))
  :config
  (defun font-lock-update-dwim ()
    (interactive)
    (if (region-active-p)
        (call-interactively #'mode-fontify-region)
      (call-interactively #'font-lock-update))))

(use-package paragraphs
  :ensure nil
  :defer nil
  :init (provide 'paragraphs)
  :custom
  (sentence-end-double-space nil))

(use-package mule-cmds
  :ensure nil
  :defer nil
  :init (provide 'mule-cmds)
  :commands (switch-input-method)
  :bind (("C-\\" . switch-input-method))
  :config
  (defcustom switch-input-method-candidates nil "Input method candidates used by `switch-input-method'.")
  (defvar switch-input-method-count 0)
  (defun switch-input-method ()
    (interactive)
    (let* ((candidates (cl-delete-if #'null (cl-remove-duplicates (cons default-input-method switch-input-method-candidates) :test #'equal)))
           (index (setf switch-input-method-count
                        (cond
                         ((null current-input-method) 0)
                         ((eq last-command 'switch-input-method) (1+ switch-input-method-count))
                         (t 1))))
           (candidates (unless (and (cl-plusp index) (zerop (mod index (length candidates)))) candidates)))
      (activate-input-method (when candidates (nth (mod (1+ (or (cl-position current-input-method candidates :test #'string=) -1)) (length candidates)) candidates))))))

(use-package menu-bar
  :ensure nil
  :defer t
  :custom (menu-bar-mode nil))

(use-package scroll-bar
  :ensure nil
  :defer t
  :custom (scroll-bar-mode nil))

(use-package tool-bar
  :ensure nil
  :defer t
  :custom (tool-bar-mode nil))

(use-package mouse
  :ensure nil
  :defer t
  :custom (context-menu-mode t))

(use-package ibuffer
  :ensure nil
  :defer t
  :init (defalias 'list-buffers 'ibuffer))

(use-package nerd-icons-ibuffer
  :when (member 'nerd-icons extra-features)
  :ensure t
  :defer t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alh")
  (browse-url-handlers '(("\\`file:" . browse-url-default-browser)))
  :hook (dired-mode . toggle-truncate-lines)
  :bind (:map dired-mode-map ("/ n" . dired-narrow))
  :config (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-aux
  :ensure nil
  :defer t
  :config
  (nconc dired-compress-files-alist '(("\\.7z\\'" . "7z a %o %i")
                                      ("\\.tar\\'" . "tar -cf %o %i"))))

(use-package dired-ext
  :load-path "custom-lisp"
  :after dired
  :demand t)

(use-package nerd-icons-dired
  :when (member 'nerd-icons extra-features)
  :ensure t
  :defer t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :after dired
  :ensure t
  :defer t
  :hook (dired-mode . diredfl-mode))

(use-package savehist
  :ensure nil
  :defer t
  :hook (minibuffer-setup . savehist-mode))

(use-package display-line-numbers
  :when (<= 26 emacs-major-version)
  :ensure nil
  :defer t
  :hook (prog-mode . display-line-numbers-mode)
  :custom (display-line-numbers-type t))

(use-package hl-line
  :ensure nil
  :defer t
  :hook (prog-mode . hl-line-mode))

(use-package xref
  :ensure nil
  :defer t
  :bind (("<mouse-8>" . xref-go-back)
         ("<mouse-9>" . xref-go-forward))
  :custom (xref-history-storage 'xref-window-local-history))

(use-package wgrep
  :ensure t
  :defer t)

(use-package compile
  :ensure nil
  :defer t
  :custom
  (compilation-scroll-output t))

(use-package comint
  :ensure nil
  :defer t
  :config
  (cl-pushnew #'comint-truncate-buffer comint-output-filter-functions))

(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(use-package vc
  :ensure nil
  :defer t
  :init
  (use-package log-view
    :ensure nil
    :defer t
    :bind (:map log-view-mode-map ("+" . vc-checkout-revision)))
  :commands (vc-checkout-revision vc-clone-repository)
  :config
  (cl-defun vc-checkout-revision (&optional (rev (when (derived-mode-p 'log-view-mode) (log-view-current-tag))))
    (interactive)
    (let* ((files (condition-case nil (cadr (vc-deduce-fileset t))
                    (t (list (vc-root-dir)))))
           (rev (or rev (vc-read-revision "Revision: " files))))
      (dolist (file files)
        (vc-checkout file rev))
      (message "Checked out revision %s" rev)))
  (defun vc-clone-repository (remote &optional directory revision backend)
    (interactive
     (let ((prefix (or (car current-prefix-arg) 1)) (args nil))
       (when (>= prefix 1)
         (push (setf remote (read-string "Repository: ")) args))
       (let ((directory (file-name-nondirectory (directory-file-name remote))))
         (if (>= prefix 4) (push (read-file-name "Directory: " nil nil nil directory) args) (push directory args)))
       (when (>= prefix 16)
         (let ((revision (read-string "Revision: ")))
           (push (unless (string-empty-p revision) revision) args)))
       (when (>= prefix 64)
         (push (vc-read-backend "Backend: ") args))
       (nreverse args)))
    (when (file-exists-p directory)
      (cl-assert (file-directory-p directory)))
    (when-let ((result (vc-clone (if (url-p remote) remote (expand-file-name remote)) backend (expand-file-name directory) revision)))
      (setf directory (or directory result))
      (message "Cloned repository %s into %s" remote directory))))

(use-package project
  :when (<= 27 emacs-major-version)
  :ensure nil
  :defer t
  :config
  (put 'compile-command 'safe-local-variable #'stringp)
  (put 'compilation-read-command 'safe-local-variable #'booleanp))

(use-package project-ext
  :load-path "custom-lisp"
  :defer t
  :bind (("C-x p u" . project-run-or-quickrun)))

(use-package winner
  :ensure nil
  :defer nil
  :config
  (winner-mode +1))

(use-package recentf
  :ensure nil
  :defer t
  :hook
  (find-file . recentf-mode)
  (recentf-mode . recentf-track-opened-file)
  :commands recentf-open-files
  :custom
  (recentf-exclude (list (rx bos (literal temporary-file-directory)) (rx "_archive" eos) #'backup-file-name-p))
  (recentf-max-saved-items 100)
  :config
  (recentf-mode +1))

(use-package tramp
  :ensure nil
  :defer t
  :custom (tramp-copy-size-limit (* 1 1024 1024))
  :config
  (use-package recentf
    :ensure nil
    :defer t
    :config (cl-pushnew tramp-file-name-regexp recentf-exclude :test #'string=))
  (define-advice tramp-open-shell (:around (fun &rest args) termux-support)
    (defvar tramp-open-shell-wait-for-output-count)
    (let ((tramp-open-shell-wait-for-output-count 0))
      (apply fun args)))
  (define-advice tramp-wait-for-output (:around (fun &rest args) termux-support)
    (if (not (boundp 'tramp-open-shell-wait-for-output-count))
        (apply fun args)
      (defvar tramp-open-shell-wait-for-output-count)
      (unless (= (cl-incf tramp-open-shell-wait-for-output-count) 1)
        (apply fun args))))
  (define-advice tramp-get-method-parameter (:filter-args (args) termux-support)
    (cl-destructuring-bind (vec param &optional default) args
      (cl-case param
        (tramp-tmpdir
         (list vec param
               (or (ignore-errors
                     (let ((tmpdir (tramp-send-command-and-read vec "echo \"\\\"$TMPDIR\\\"\"")))
                       (unless (string-empty-p tmpdir) tmpdir)))
                   default)))
        (t args))))
  (cl-pushnew "/data/data/com.termux/files/usr/bin" tramp-remote-path :test #'equal)
  (connection-local-set-profile-variables 'remote-direct-async-process '((tramp-direct-async-process . t)))
  (cl-loop for protocol in '("scp" "ssh")
           do (connection-local-set-profiles
               `(:application tramp :protocol ,protocol)
               'remote-direct-async-process)))

(use-package repeat
  :when (<= 28 emacs-major-version)
  :ensure nil
  :defer t
  :init
  (defun require-repeat ()
    (require 'repeat)
    (remove-hook 'pre-command-hook #'require-repeat)
    (fmakunbound #'require-repeat))
  :hook (pre-command . require-repeat)
  :config
  (repeat-mode +1))

(use-package picture
  :ensure nil
  :defer t
  :bind (:map picture-mode-map ("C-c v" . picture-movement-down)))

(use-package calc
  :ensure nil
  :defer t)

(use-package calc-bin-ext
  :load-path "custom-lisp"
  :demand t
  :after calc)

;;;;;;;;;;;;;;;;;
;; Interaction ;;
;;;;;;;;;;;;;;;;;

(use-package ace-window
  :ensure t
  :defer t
  :bind (("M-o" . ace-window))
  :custom (aw-dispatch-always t))

(use-package hydra
  :ensure t
  :defer t)

(use-package pretty-hydra
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :defer t
  :init
  (defun require-which-key ()
    (require 'which-key)
    (fmakunbound #'require-which-key)
    (remove-hook 'pre-command-hook #'require-which-key))
  :hook (pre-command . require-which-key)
  :config
  (which-key-mode t))

(use-package nerd-icons
  :when (member 'nerd-icons extra-features)
  :ensure t
  :defer nil)

(use-package doom-modeline
  :ensure t
  :defer nil
  :custom (doom-modeline-icon (member 'nerd-icons extra-features))
  :config
  (doom-modeline-mode +1))

(use-package mode-line-bell
  :ensure t
  :defer nil
  :custom
  (mode-line-bell-flash-time 0.1)
  :config
  (mode-line-bell-mode +1))

(use-package hide-mode-line
  :ensure t
  :defer t)

(use-package vertico
  :when (<= 27 emacs-major-version)
  :ensure t
  :defer nil
  :config
  (vertico-mode +1))

(use-package vertico-mouse
  :after vertico
  :ensure nil
  :demand t
  :config
  (vertico-mouse-mode +1))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :defer t
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("C-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package company
  :ensure t
  :defer t
  :custom
  (company-minimum-prefix-length 1)
  (company-frontends '(company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-backends '(company-capf))
  :init
  (use-package company
    :when (> 27 emacs-major-version)
    :hook ((prog-mode ielm-mode tex-mode sly-mode racket-repl-mode) . company-mode))
  :config
  (use-package company
    :after tex
    :demand t
    :config (add-hook 'TeX-mode-hook (lambda () (delete 'company-dabbrev company-backends)))))

(use-package corfu
  :when (<= 27 emacs-major-version)
  :ensure t
  :defer t
  :hook
  ((prog-mode ielm-mode tex-mode sly-mode racket-repl-mode geiser-repl-mode) . corfu-mode)
  (corfu-mode . corfu-popupinfo-mode)
  :bind (:map corfu-map
         ("C-M-i" . corfu-move-to-minibuffer)
         ("M-." . corfu-popupinfo-toggle))
  :custom
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-on-exact-match nil)
  (corfu-max-width 60)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay nil)
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred))))))

(use-package corfu-terminal
  :when (<= 27 emacs-major-version)
  :ensure t
  :unless (display-graphic-p)
  :defer t
  :hook (corfu-mode . corfu-terminal-mode))

(use-package cape
  :when (<= 27 emacs-major-version)
  :ensure t
  :defer t
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :custom
  (cape-dabbrev-check-other-buffers nil)
  (cape-dabbrev-min-length 2)
  :init
  (push #'cape-file completion-at-point-functions)
  (push #'cape-tex completion-at-point-functions)
  (push #'cape-dabbrev completion-at-point-functions)
  (push #'cape-keyword completion-at-point-functions))

(use-package nerd-icons-corfu
  :when (member 'nerd-icons extra-features)
  :ensure t
  :demand t
  :after corfu
  :config
  (unless corfu-margin-formatters
    (cl-pushnew #'nerd-icons-corfu-formatter corfu-margin-formatters)))

(use-package dabbrev
  :ensure nil
  :defer t
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-check-all-buffers nil))

(use-package orderless
  :when (<= 26 emacs-major-version)
  :ensure t
  :defer t
  :custom (orderless-matching-styles '(orderless-prefixes))
  :hook
  ((minibuffer-setup corfu-mode) . (lambda () (setq-local completion-styles '(orderless basic))))
  :config
  (defun orderless-literal-when-nonascii (pattern _index _total)
    (when (string-match "[^[:ascii:]]" pattern) #'orderless-literal))
  (defun orderless-literal-when-begin-with-upper-case (pattern _index _total)
    (when (let ((case-fold-search nil)) (string-match "^[[:upper:]]" pattern)) #'orderless-literal))
  (defun orderless-regexp-when-regexp-symbol (pattern _index _total)
    (when (string-match "[\\^$?\\.+*^]\\|\\[\\|\\]" pattern) #'orderless-regexp))
  (push #'orderless-literal-when-nonascii orderless-style-dispatchers)
  (push #'orderless-literal-when-begin-with-upper-case orderless-style-dispatchers)
  (push #'orderless-regexp-when-regexp-symbol orderless-style-dispatchers))

(use-package marginalia
  :when (<= 27 emacs-major-version)
  :ensure t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :defer t
  :hook (minibuffer-setup . marginalia-mode))

(use-package nerd-icons-completion
  :when (member 'nerd-icons extra-features)
  :ensure t
  :defer t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config (nerd-icons-completion-mode +1))

(use-package consult
  :when (<= 27 emacs-major-version)
  :ensure t
  :defer t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-x M-x" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (use-package minibuffer
    :ensure nil
    :custom (completion-in-region-function #'consult-completion-in-region))
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (require 'recentf)
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "M-.")
  (setq consult-narrow-key "<")
  (use-package consult-org
    :after org
    :defer t
    :bind (:map org-mode-map ("M-g i" . consult-org-heading))))

(use-package consult-dir
  :when (<= 27 emacs-major-version)
  :ensure t
  :defer t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :when (<= 26 emacs-major-version)
  :ensure t
  :defer t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (cl-pushnew '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                nil
                (window-parameters (mode-line-format . none)))
              display-buffer-alist)
  (define-key embark-function-map (kbd "M-d") #'disassemble))

(use-package embark-consult
  :when (<= 27 emacs-major-version)
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package tempel
  :when (<= 27 emacs-major-version)
  :ensure t
  :defer t
  :custom
  (tempel-trigger-prefix "<")
  :hook
  ((text-mode prog-mode minibuffer-setup) . tempel-setup-capf)
  ((text-mode prog-mode minibuffer-setup) . tempel-tab-mode)
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :commands (tempel-tab-mode)
  :config
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions)))
  (defconst tempel-maybe-expand `(menu-item "" ,(lambda () (interactive) (tempel-expand t)) :filter ,(lambda (cmd) (when (tempel-expand nil) cmd))))
  (defvar tempel-tab-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "TAB") tempel-maybe-expand)
      map)
    "Keymap for `tempel-tab-mode'.")
  (define-minor-mode tempel-tab-mode
    "Minor mode to make tempel use <tab> for template expansion as well as `tempel-next', and use <backtab> for `tempel-previous'."
    :group 'tempel
    :keymap tempel-tab-mode-map
    (if tempel-tab-mode
        (progn (define-key tempel-map (kbd "TAB") #'tempel-next)
               (define-key tempel-map (kbd "<backtab>") #'tempel-previous))
      (define-key tempel-map (kbd "TAB") nil)
      (define-key tempel-map (kbd "<backtab>") nil))))

(use-package crux
  :ensure t
  :defer t
  :bind (:map global-map
         ("C-x C-S-e" . crux-eval-and-replace)
         ("C-x C-S-f" . crux-open-with)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-<return>" . crux-smart-open-line)
         ("S-<f1>" . crux-find-user-init-file)))

(use-package vlf
  :ensure t
  :defer t
  :init (require 'vlf-setup))

(use-package cnfonts
  :when (and (display-graphic-p) (member 'cnfonts extra-features))
  :ensure t
  :defer nil
  :bind (("C-M-_" . cnfonts-decrease-fontsize)
         ("C-M-+" . cnfonts-increase-fontsize)
         ("C-M-)" . cnfonts-reset-fontsize))
  :custom
  (cnfonts-personal-fontnames '(("Jetbrains Mono") ("MiSans VF") nil nil))
  (cnfonts-profiles '("program" "document"))
  (cnfonts-use-face-font-rescale t)
  (cnfonts-use-cache t)
  :config
  (cnfonts-mode +1))

(use-package popper
  :when (<= 26 emacs-major-version)
  :defer nil
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :commands popper-delete-other-windows
  :custom
  (popper-reference-buffers '("\\*Messages\\*"
                              "Output\\*$"
                              "out\\*$"
                              "REPL\\*$"
                              "\\*ielm\\*"
                              "\\*Async Shell Command\\*"
                              "\\*rustic-compilation\\*"
                              "\\*cargo-run\\*"
                              "\\*gt-result\\*"
                              "\\*Compile-Log\\*"
                              "^\\*lsp-install"
                              "^\\*sly-mrepl"
                              dap-ui-repl-mode
                              help-mode
                              compilation-mode
                              dap-server-log-mode
                              xref--xref-buffer-mode
                              quickrun--mode
                              flymake-diagnostics-buffer-mode
                              flymake-project-diagnostics-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  (popper-mode-line '(:eval (propertize " POP " 'face 'mode-line-emphasis)))
  :config
  (defun delete-other-windows@before (&optional window &rest _)
    (let ((buffer (window-buffer window)))
      (when (popper-display-control-p buffer)
        (popper-toggle-type buffer))))
  (advice-add #'delete-other-windows :before #'delete-other-windows@before)
  
  (defun popper--fit-window-height (win)
      (fit-window-to-buffer
         win
         (floor (frame-height) 3)
         (floor (frame-height) 4)))

  (defun popper-popup-buffer (buffer)
    (unless (window-dedicated-p (selected-window))
      (switch-to-buffer buffer))
    (popper-lower-to-popup buffer))
  
  (use-package project
    :when (<= 27 emacs-major-version)
    :ensure nil
    :defer t
    :config
    (defun project-eshell@around (fun &rest _)
      (popper-popup-buffer (funcall fun)))
    (advice-add #'project-eshell :around #'project-eshell@around))
  
  (use-package eshell
    :ensure nil
    :defer t
    :bind ("C-<tab>" . eshell-popper-request)
    :config
    (cl-pushnew 'eshell-tramp eshell-modules-list)
    (defun eshell-popper-buffer-p (buffer)
        (and (eq (with-current-buffer buffer major-mode) 'eshell-mode) (popper-display-control-p buffer) (string-match-p "\\*eshell-popper\\*\\(<[0-9]+>\\)\\{0,1\\}" (buffer-name buffer))))    
    (defun eshell-popper-request ()
      (interactive)
      (if (eshell-popper-buffer-p (current-buffer))
          (delete-window)
        (let ((request-default-directory default-directory)
              (eshell-buffer-name "*eshell-popper*"))
          (if (catch 'break
                (dolist (buffer (buffer-list))
                  (when (and (eshell-popper-buffer-p buffer) (not (get-buffer-process buffer)))
                    (popper-popup-buffer buffer)
                    (throw 'break t))))
              (unless (string-equal (expand-file-name default-directory) (expand-file-name request-default-directory))
                (progn (eshell/cd request-default-directory)
                       (eshell-interrupt-process)))
            (popper-lower-to-popup (eshell t))))))))

(use-package smartparens
  :ensure t
  :defer t
  :init
  (defalias 'sp-mode #'smartparens-mode)
  :hook
  ((prog-mode text-mode minibuffer-setup eshell-mode lisp-mode scheme-mode ielm-mode sly-mrepl-mode racket-repl-mode inferior-scheme-mode geiser-repl-mode) . smartparens-mode)
  (smartparens-mode . show-smartparens-mode)
  :bind (:map smartparens-mode-map
         ("C-*"               . sp-join-sexp)
         ("C-|"               . sp-split-sexp)
         ("C-M-f"             . sp-forward-sexp)
         ("C-M-b"             . sp-backward-sexp)
         ("C-M-d"             . sp-down-sexp)
         ("C-M-S-d"           . sp-backward-down-sexp)
         ("C-S-a"             . sp-beginning-of-sexp)
         ("C-S-e"             . sp-end-of-sexp)
         ("C-M-u"             . sp-up-sexp)
         ("C-M-S-u"           . sp-backward-up-sexp)
         ("C-M-n"             . sp-next-sexp)
         ("C-M-p"             . sp-previous-sexp)
         ("C-M-k"             . sp-kill-sexp)
         ("C-M-\""            . sp-backward-unwrap-sexp)
         ("C-\""              . sp-unwrap-sexp)
         ("M-\""              . sp-rewrap-sexp)
         ("C-M-\""            . sp-splice-sexp)
         ("C-)"               . sp-select-next-thing-exchange)
         ("C-M-)"             . sp-select-next-thing)
         ("C-("               . sp-select-previous-thing-exchange)
         ("C-M-("             . sp-select-previous-thing)
         ("C-M-SPC"           . sp-mark-sexp)
         :map emacs-lisp-mode-map
         ("M-<right>"         . sp-forward-slurp-sexp)
         ("M-<left>"          . sp-forward-barf-sexp)
         ("C-M-t"             . sp-transpose-sexp)
         ("C-M-S-t"           . sp-convolute-sexp)
         ("C-M-<left>"        . sp-backward-slurp-sexp)
         ("C-M-<right>"       . sp-backward-barf-sexp)
         ("C-M-<delete>"      . sp-splice-sexp-killing-forward)
         ("C-M-<backspace>"   . sp-splice-sexp-killing-backward)
         ("C-M-S-<backspace>" . sp-splice-sexp-killing-around)
         ("M-F"               . sp-forward-symbol)
         ("M-B"               . sp-backward-symbol))
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-c-modes '(c-mode c-ts-mode
                c++-mode c++-ts-mode
                objc-mode objc-ts-mode
                java-mode java-ts-mode
                scala-mode scala-ts-mode
                rust-mode rust-ts-mode
                rustic-mode rustic-ts-mode
                js-mode js-ts-mode
                dart-mode dart-ts-mode
                scad-mode scad-ts-mode
                typescript-mode typescript-ts-mode))
  :config
  (require 'smartparens-config)
  (sp-pair "（" "）")
  (sp-pair "【" "】")
  (sp-pair "《" "》")
  (sp-pair "「" "」")
  (sp-pair "“" "”")
  (sp-pair "’" "’")
  (use-package smartparens
    :after org
    :demand t
    :config
    (sp-local-pair 'org-mode "\\[" "\\]")
    (sp-local-pair 'org-mode "<<" ">>")
    (sp-local-pair 'org-mode "@@" "@@")
    (sp-local-pair 'org-mode "+" "+" :unless '(sp-point-after-word-p)))
  (use-package smartparens
    :after lisp-mode
    :demand t
    :config
    (sp-local-pair 'lisp-mode "|" "|")
    (sp-local-pair 'lisp-mode "#|" "|#"))
  (use-package smartparens
    :after eshell
    :demand t
    :config
    (defun sp-point-in-eshell-sexp (&rest _)
      (let ((after-prompt-position (save-excursion (eshell-bol) (and (looking-back eshell-prompt-regexp (line-beginning-position)) (point))))
            (sexp-beg (save-excursion (elisp--beginning-of-sexp) (point))))
        (>= sexp-beg (or after-prompt-position (line-beginning-position)))))
    (sp-local-pair 'eshell-mode "#<" ">")
    (sp-local-pair 'eshell-mode "'" "'" :unless '(sp-point-in-eshell-sexp))))

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :custom
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-odd-face-perc 15)
  (highlight-indent-guides-auto-even-face-perc 25)
  (highlight-indent-guides-auto-top-odd-face-perc 40)
  (highlight-indent-guides-auto-top-even-face-perc 45)
  (highlight-indent-guides-auto-character-face-perc 40)
  (highlight-indent-guides-auto-stack-odd-face-perc 35)
  (highlight-indent-guides-auto-stack-even-face-perc 40)
  (highlight-indent-guides-auto-top-character-face-perc 75)
  (highlight-indent-guides-auto-stack-character-face-perc 70)
  :hook ((python-mode toml-mode yaml-mode haskell-mode lua-mode ruby-mode octave-mode matlab-mode) . highlight-indent-guides-mode))

(use-package string-inflection
  :ensure t
  :defer t
  :bind (:map prog-mode-map
         ("C-c i i" . string-inflection-cycle)
         ("C-c i t" . string-inflection-toggle)
         ("C-c i C" . string-inflection-camelcase)
         ("C-c i c" . string-inflection-lower-camelcase)
         ("C-c i -" . string-inflection-kebab-case)
         ("C-c i _" . string-inflection-underscore))
  :config
  (defvar string-inflection-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "i") #'string-inflection-cycle)
      (define-key map (kbd "t") #'string-inflection-toggle)
      (dolist (it '(string-inflection-cycle string-inflection-toggle))
        (put it 'repeat-map 'string-inflection-repeat-map))
      map)
    "Keymap to repeat string inflection key sequences.  Used in `repeat-mode'."))

(use-package indent-yank
  :quelpa (indent-yank :fetcher github :repo "bohonghuang/indent-yank")
  :defer t
  :commands (indent-yank-yank)
  :hook (prog-mode . indent-yank-mode))

(use-package separedit
  :ensure t
  :defer t
  :bind
  (:map prog-mode-map
   ("C-c '" . separedit)
   :map minibuffer-local-map
   ("C-c '" . separedit)
   :map help-mode-map
   ("C-c '" . separedit)))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook
  ((lisp-mode lisp-data-mode emacs-lisp-mode scheme-mode inferior-scheme-mode racket-mode clojure-mode racket-repl-mode sly-mrepl-mode geiser-repl-mode) . rainbow-delimiters-mode))

(use-package drag-stuff
  :ensure t
  :defer t
  :diminish
  :commands drag-stuff-mode drag-stuff-global-mode
  :bind (:map drag-stuff-mode-map
         ("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)
         ("M-<right>" . drag-stuff-right)
         ("M-<left>" . drag-stuff-left))
  :hook (prog-mode . (lambda () (unless (-contains-p '(emacs-lisp-mode lisp-mode common-lisp-mode) major-mode) (drag-stuff-mode +1)))))

(use-package pixel-scroll
  :when (<= 29 emacs-major-version)
  :ensure nil
  :defer nil
  :custom
  (pixel-scroll-precision-use-momentum t)
  (pixel-scroll-precision-momentum-seconds 0.5)
  :config
  (pixel-scroll-precision-mode +1))

(use-package good-scroll
  :when (and (display-graphic-p) (<= 27 emacs-major-version 28))
  :ensure t
  :defer nil
  :custom (good-scroll-step 100)
  :config
  (good-scroll-mode +1))

(use-package elisp-mode
  :ensure nil
  :defer t
  :init (defalias 'elisp-mode 'emacs-lisp-mode))

(use-package ielm
  :ensure nil
  :defer t
  ;; Adapted from http://www.modernemacs.com/post/comint-highlighting/ to add
  ;; syntax highlighting to ielm REPLs.
  :config (setq ielm-font-lock-keywords
           (append '(("\\(^\\*\\*\\*[^*]+\\*\\*\\*\\)\\(.*$\\)"
                      (1 font-lock-comment-face)
                      (2 font-lock-constant-face)))
                   (when (require 'highlight-numbers nil t)
                     (highlight-numbers--get-regexp-for-mode 'emacs-lisp-mode))
                   (cl-loop for (matcher . match-highlights)
                            in (append lisp-el-font-lock-keywords-2
                                       lisp-cl-font-lock-keywords-2)
                            collect
                            `((lambda (limit)
                                (when ,(if (symbolp matcher)
                                           `(,matcher limit)
                                         `(re-search-forward ,matcher limit t))
                                  ;; Only highlight matches after the prompt
                                  (> (match-beginning 0) (car comint-last-prompt))
                                  ;; Make sure we're not in a comment or string
                                  (let ((state (syntax-ppss)))
                                    (not (or (nth 3 state)
                                             (nth 4 state))))))
                              ,@match-highlights)))))

(use-package macrostep
  :ensure t
  :defer t
  :bind (:map emacs-lisp-mode-map
         ("C-c C-<return>" . macrostep-expand)))

(use-package flymake
  :ensure nil
  :defer t
  :hook (emacs-lisp-mode . flymake-mode))

(use-package flymake-popon
  :when (<= 26 emacs-major-version)
  :ensure t
  :defer t
  :hook (flymake-mode . flymake-popon-mode)
  :custom
  (flymake-popon-delay 0.5)
  (flymake-popon-posframe-extra-arguments '(:poshandler posframe-poshandler-point-bottom-left-corner)))

(use-package subword
  :ensure nil
  :defer t
  :hook ((c-mode c++-mode objc-mode java-mode scala-mode rustic-mode python-mode js-mode typescript-mode) . subword-mode))

(use-package hideshow
  :ensure nil
  :defer t
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
         ("<backtab>" . hs-toggle-hiding-all))
  :config
  (defvar hs-last-looking-position nil)
  (defun hs-togglable-p (&optional cmd)
    (when hs-minor-mode
      (let ((at-indentation-p (when (looking-back (rx bol (* blank)) (line-beginning-position)) (match-beginning 0))))
        (and
         (or (and (or (eq last-command 'hs-toggle-hiding-at-point)
                      (eq last-command (or (keymap-local-lookup "TAB") (keymap-global-lookup "TAB"))))
                  at-indentation-p (save-excursion (goto-char at-indentation-p) (hs-looking-at-block-start-p)))
             (hs-looking-at-block-start-p))
         (not (region-active-p))
         (or (not at-indentation-p)
             (eq (point) (save-excursion (back-to-indentation) (point))))
         (or cmd (point))))))
  (defun hs-toggle-hiding-at-point (&optional arg)
    (interactive "P")
    (save-excursion (hs-toggle-hiding)))
  (defvar hs-all-hidden-p nil)
  (defun hs-toggle-hiding-all ()
    (interactive)
    (if hs-all-hidden-p
        (hs-show-all)
      (hs-hide-all))
    (setq-local hs-all-hidden-p (not hs-all-hidden-p)))
  (defconst hs-maybe-toggle-hideing-at-point '(menu-item "" hs-toggle-hiding-at-point :filter hs-togglable-p))
  (define-key hs-minor-mode-map (kbd "TAB") hs-maybe-toggle-hideing-at-point))

(use-package language-support
  :load-path "modules"
  :demand t
  :bind ("C-c l" . language-support-enable))

(use-package android-support
  :when (string= system-type "android")
  :load-path "modules"
  :demand t)

(use-package ai-support
  :load-path "modules"
  :demand t)

(use-package magit
  :defer t
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-bury-buffer-function #'quit-window)
  (magit-tramp-pipe-stty-settings 'pty)
  :bind (("C-x g" . magit)
         ("C-x G" . magit-dispatch)))

(use-package binary-jump
  :load-path "custom-lisp"
  :defer t
  :bind (("M-P" . binary-jump-previous-line)
         ("M-N" . binary-jump-next-line)
         ("M-J" . binary-jump-select-line-command)))

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-=" . er/expand-region))
  :config
  (use-package treesit
    :when (<= 29 emacs-major-version)
    :config
    (defun treesit-expand-region ()
      (let* ((root (treesit-buffer-root-node))
             (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
             (node-start (treesit-node-start node))
             (node-end (treesit-node-end node)))
        ;; Node fits the region exactly. Try its parent node instead.
        (when (and (= (region-beginning) node-start) (= (region-end) node-end))
          (when-let ((node (treesit-node-parent node)))
            (setf node-start (treesit-node-start node)
                  node-end (treesit-node-end node))))
        (set-mark node-end)
        (goto-char node-start)))
    (cl-pushnew #'treesit-expand-region (default-value 'er/try-expand-list))
    (cl-pushnew #'treesit-expand-region er/try-expand-list)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" . mc/skip-to-previous-like-this)))

(use-package phi-search
  :ensure t
  :defer t)

(use-package isearch-mb
  :when (<= 27 emacs-major-version)
  :ensure t
  :after isearch
  :demand t
  :config
  (isearch-mb-mode +1))

(use-package intellij-edit
  :load-path "custom-lisp"
  :defer t
  :hook
  ((c-mode c++-mode objc-mode java-mode scala-mode rust-mode rustic-mode js-mode dart-mode scad-mode js-mode typescript-mode) . intellij-edit-cc-mode)
  (python-mode . intellij-edit-python-mode)
  :commands (intellij-edit-cc-mode intellij-edit-indent-mode))

(use-package time
  :ensure nil
  :defer t
  :custom
  (display-time-string-forms '(24-hours ":" minutes)))

(use-package calendar
  :ensure nil
  :defer t
  :custom
  (calendar-mark-today t)
  (calendar-mark-holidays-flag t)
  (calendar-chinese-all-holidays-flag t))

(use-package calendar-ext
  :load-path "custom-lisp"
  :demand t
  :after calendar)

(use-package cal-china-x
  :ensure t
  :demand t
  :after calendar
  :config
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")
                                       (holiday-lunar 12 8 "腊八节")
                                       (holiday-fixed 2 14 "情人节")
                                       (holiday-fixed 3 8 "妇女节")
                                       (holiday-fixed 4 1 "愚人节")
                                       (holiday-fixed 9 10 "教师节")
                                       (holiday-fixed 11 26 "感恩节")
                                       (holiday-fixed 11 1 "万圣节")
                                       (holiday-fixed 12 25 "圣诞节")
                                       (holiday-fixed 12 24 "平安夜")
                                       (holiday-month-week-day 6 3 7 "父亲节")
                                       (holiday-month-week-day 5 2 7 "母亲节")))
  (setq calendar-holidays (append cal-china-x-chinese-holidays cal-china-x-general-holidays cal-china-x-important-holidays)))

;;;;;;;;;
;; Org ;;
;;;;;;;;;

(use-package org
  :ensure nil
  :defer t
  :custom
  (org-adapt-indentation nil)
  (org-hide-emphasis-markers t)
  (org-default-notes-file (expand-file-name "org-capture/captures.org" org-directory))
  (org-highlight-latex-and-related '(native))
  (org-startup-folded 'showall)
  :bind (:map org-mode-map
              ("M-," . org-mark-ring-goto)
              ("M-." . org-open-at-point)))

(use-package org-src
  :ensure nil
  :defer t
  :custom
  (org-src-window-setup 'current-window)
  :config
  (defun org-latex-disable-global-capf (&rest _)
    (when (local-variable-p 'completion-at-point-functions)
      (setq completion-at-point-functions (cl-remove-if #'booleanp completion-at-point-functions))))
  (advice-add #'org-edit-latex-environment :after #'org-latex-disable-global-capf)
  (advice-add #'org-edit-latex-fragment :after #'org-latex-disable-global-capf))

(use-package org-attach
  :after org
  :demand t
  :custom
  (org-attach-use-inheritance t)
  (org-attach-id-dir (expand-file-name "org-attach/data" org-directory)))

(use-package org-attach-refactor
  :quelpa (org-attach-refactor :fetcher github :repo "bohonghuang/org-attach-refactor")
  :defer t
  :commands (org-attach-refactor-remove-id org-attach-refactor-add-id))

(use-package org-ext
  :load-path "custom-lisp"
  :after org
  :demand t
  :bind (:map org-mode-map
         ("C-c C-S-L" . org-link-make-from-region)))

(use-package org-agenda
  :ensure nil
  :defer t
  :bind (:map org-mode-map
         ("C-c C-x C-S-o" . org-resolve-clocks)
         :map org-agenda-mode-map
         ("C-c C-x C-S-o" . org-resolve-clocks)
         ("C-c C-x C-q" . org-clock-cancel))
  :custom
  (org-agenda-files (list (expand-file-name "org-agenda" org-directory)))
  (org-agenda-window-setup 'current-window)
  :config
  (require 'recentf)
  (require 'org-gtd nil t)
  (nconc recentf-exclude (org-agenda-files))
  (use-package appt
    :when (member 'appt-org-agenda extra-features)
    :demand t
    :config
    (add-hook 'org-agenda-finalize-hook #'org-agenda-to-appt)
    (appt-activate +1)))

(use-package appt
  :ensure nil
  :defer t
  :custom
  (appt-message-warning-time 6)
  (appt-display-interval 3)
  (appt-display-format 'window)
  (appt-disp-window-function #'appt-disp-window-and-notification)
  :config
  (defun appt-disp-window-and-notification (min-to-appt new-time appt-msg)
    (require 'notifications)
    (notifications-notify :timeout (* appt-display-interval 60000)
                          :title (format "You have a task in %s minutes" min-to-appt)
                          :body (string-trim (replace-regexp-in-string "\\([0-9]\\{1,2\\}:[0-9]\\{1,2\\}\\)\\(-[0-9]\\{1,2\\}:[0-9]\\{1,2\\}\\)\\{0,1\\}" "" (substring-no-properties appt-msg)))
                          :sound-name "alarm-clock-elapsed")    
    (ignore-errors
      (appt-disp-window min-to-appt new-time appt-msg))))

(use-package org-edna
  :when (member 'org-gtd extra-features)
  :ensure t
  :defer t
  :custom (org-edna-use-inheritance t)
  :config (org-edna-mode +1))

(use-package org-gtd
  :when (member 'org-gtd extra-features)
  :ensure t
  :defer t
  :custom
  (org-gtd-directory (expand-file-name "org-gtd" org-directory))
  :bind
  (("C-c g c" . org-gtd-capture)
   ("C-c g e" . org-gtd-engage)
   ("C-c g p" . org-gtd-process-inbox)
   ("C-c g n" . org-gtd-show-all-next)
   ("C-c g s" . org-gtd-show-stuck-projects)
   ("C-c g a" . org-agenda-list)
   :map org-gtd-clarify-map
   ("C-c C-c" . org-gtd-organize))
  :config
  (setf org-gtd-default-file-name "tasks")
  (cl-pushnew org-gtd-directory org-agenda-files))

(use-package buffer-timer
  :when (member 'org-gtd extra-features)
  :quelpa (buffer-timer :fetcher github :repo "bohonghuang/elisp-buffer-timer")
  :defer t
  :commands (buffer-timer-start buffer-timer-stop)
  :bind (;; reporting
         ("C-c g t s" . buffer-timer-summarize)
         ("C-c g t r" . buffer-timer-report)
         ("C-c g t S" . buffer-timer-write-results)
         ("C-c g t c" . buffer-timer-clear)
         ("C-c g t m" . buffer-timer-munge)
         ("C-c g t M" . buffer-timer-munge-date-range)
         ;; modifying data
         ("C-c g t t" . buffer-timer-transfer-time)
         ("C-c g t a" . buffer-timer-adjust-time)
         ("C-c g t A" . buffer-timer-adjust-older-time)
         ;; locking to a subject
         ("C-c g t i" . buffer-timer-toggle-idle)
         ("C-c g t l" . buffer-timer-lock)
         ("C-c g t u" . buffer-timer-unlock)
         ("C-c g t U" . buffer-timer-do-idle-calculations)
         ("C-c g t L" . buffer-timer-view-log)))

(use-package org-capture
  :ensure nil
  :defer t
  :custom
  (org-capture-templates `(("p" "Protocol" entry (file+function ,(expand-file-name "org-capture/inbox.org" org-directory)
                                                                ,(lambda () (org-goto-or-insert-heading (org-capture-get :annotation))))
                            "* %^{Title}\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?\n\nCaptured On: %U")
  	                   ("P" "Protocol Link" entry (file ,(expand-file-name "org-capture/inbox.org" org-directory))
                            "* %? [[%:link][%:description]] \nCaptured On: %U"))))

(use-package org-protocol
  :ensure nil
  :demand t
  :after org-capture)

(use-package org-download
  :defer t
  :ensure t
  :hook (org-mode . org-download-enable)
  :custom
  (org-download-display-inline-images nil)
  (org-download-method 'attach))

(use-package org-appear
  :ensure t
  :defer t
  :hook (org-mode . org-appear-mode))

(use-package org-remark
  :when (member 'org-remark extra-features)
  :init (require 'org-remark-global-tracking)
  :ensure t
  :defer t
  :custom
  (org-remark-global-tracking-mode +1)
  (org-remark-notes-file-name #'org-remark-notes-auto-file-name)
  (org-remark-create-default-pen-set nil)
  (org-remark-notes-display-buffer-action '((display-buffer-in-side-window)
                                            (side . right)
                                            (slot . 1)))
  :bind (("C-c n m" . org-remark-mark)
         :map org-remark-mode-map
         ("C-c n o" . org-remark-open)
         ("C-c n n" . org-remark-view-next)
         ("C-c n p" . org-remark-view-prev)
         ("C-c n r" . org-remark-remove)
         ("C-c n k" . org-remark-delete)
         ("C-c n c" . org-remark-change)
         ("C-c n ." . org-remark-view))
  :commands (org-remark-notes-auto-file-name)
  :config
  (defun org-remark-notes-auto-file-name ()
    (if buffer-file-name
        (concat (file-name-sans-extension
                 (file-name-nondirectory (org-remark-source-find-file-name)))
                ".remark.org")
      (expand-file-name "org-remark/notes.org" org-directory)))
  (org-remark-create "red"
                     '(:underline "red" :background "dark red")
                     '(CATEGORY "important"))
  (org-remark-create "blank"
                     `(:underline ,(face-foreground 'default) :foreground ,(face-background 'default)))
  (defvar org-remark-repeat-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "n") #'org-remark-view-next)
          (define-key map (kbd "p") #'org-remark-view-prev)
          (define-key map (kbd ".") #'org-remark-view)
          (dolist (it '(org-remark-view-next org-remark-view-prev org-remark-view))
            (put it 'repeat-map 'org-remark-repeat-map))
          map)
        "Keymap to repeat note navigation key sequences.  Used in `repeat-mode'."))

(use-package org-noter
  :when (member 'org-noter extra-features)
  :ensure t
  :defer t
  :custom
  (org-noter-default-notes-file-names '("notes.org"))
  (org-noter-notes-search-path (list (expand-file-name "org-noter" org-directory)))
  (org-noter-always-create-frame nil))

(use-package pdf-tools
  :when (member 'pdf-tools extra-features)
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . (lambda ()
                         (require 'pdf-tools)
                         (pdf-tools-install)
                         (pdf-view-mode)))
  :bind (:map pdf-view-mode-map
         ("M-g M-g" . pdf-view-goto-page)
         ("M-g g" . pdf-view-goto-page)))

(use-package org-pdftools
  :when (and (member 'org-noter extra-features) (member 'pdf-tools extra-features))
  :ensure t
  :defer t
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :when (and (member 'org-noter extra-features) (member 'pdf-tools extra-features))
  :after pdf-tools
  :init
  (use-package org-noter-pdftools
    :after (pdf-tools org-noter)
    :demand t)
  :ensure t
  :defer t
  :bind (:map pdf-view-mode-map ("i" . org-noter)))

(use-package org-roam
  :when (member 'org-roam extra-features)
  :ensure t
  :defer t
  :custom
  (org-roam-directory (expand-file-name "org-roam" org-directory))
  (org-roam-graph-link-den-types '("file" "attachment"))
  :bind (("C-c r t" . org-roam-buffer-toggle)
         ("C-c r n" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ("C-c r j" . org-roam-dailies-capture-today))
  :hook (org-mode . (lambda () (require 'org-roam)))
  :config
  (require 'org-roam-protocol)
  (org-roam-db-autosync-mode +1))

(use-package org-journal
  :when (member 'org-journal extra-features)
  :ensure t
  :defer t
  :init
  (setq org-journal-prefix-key "C-c j")
  :bind (("C-c j j" . org-journal-new-entry))
  :custom
  (org-journal-date-format "%Y 年 %m 月 %d 日 %A")
  (org-journal-dir (expand-file-name "org-journal" org-directory))
  :config
  (global-unset-key (kbd "C-c C-j"))
  (defun org-journal-insert-template-after-header ()
    (let ((template-file (expand-file-name ".template-header.org")))
      (when (file-exists-p template-file)
        (org-return)
        (insert-file-contents template-file)
        (when (org-in-src-block-p)
          (org-indent-block)
          (make-local-variable 'org-confirm-babel-evaluate)
          (setq org-confirm-babel-evaluate nil)
          (org-babel-execute-src-block)
          (kill-local-variable 'org-confirm-babel-evaluate)))))
  (add-hook 'org-journal-after-header-create-hook #'org-journal-insert-template-after-header))

(use-package org-bars
  :when (<= 27 emacs-major-version)
  :quelpa (org-bars :fetcher github :repo "bohonghuang/org-bars")
  :defer t
  :hook (org-mode . org-bars-mode)
  :config
  (use-package org-tree-slide-ext
    :when (member 'org-tree-slide extra-features)
    :load-path "custom-lisp"
    :demand t
    :config
    (push '(org-bars-mode . nil) org-tree-slide-minor-mode-alist)))

(use-package org-indent
  :ensure nil
  :defer t
  :hook (org-mode . org-indent-mode))

(use-package ob
  :ensure nil
  :defer t
  :config
  (defun org-babel-check-evaluate@before (info)
    (ignore-errors (org-babel-do-load-languages 'org-babel-load-languages (list (cons (intern (car info)) t)))))
  (advice-add #'org-babel-check-evaluate :before #'org-babel-check-evaluate@before))

(use-package ob-ditaa
  :defer t
  :config
  (defun ob-ditaa-fix-nonascii-before-execute (args)
    (pcase-let ((`(,body ,params) args))
      (list (replace-regexp-in-string "\\([^[:ascii:]]\\)" "\\1 " body) params)))
  (advice-add #'org-babel-execute:ditaa :filter-args #'ob-ditaa-fix-nonascii-before-execute))

(use-package ob-svgbob
  :when (member 'svgbob language-support-languages)
  :ensure t
  :defer t
  :init
  (defalias 'svgbob-mode 'artist-mode))

(use-package ox
  :ensure nil
  :defer t
  :custom
  (org-export-with-tags nil))

(use-package ox-latex
  :ensure nil
  :defer t
  :init
  (use-package org
    :defer t
    :config
    (cl-pushnew
     `(imagemagick-xelatex
       :programs ("xelatex" "convert")
       :description "pdf > png"
       :message "you need to install the programs: xelatex and imagemagick."
       :image-input-type "pdf"
       :image-output-type "png"
       :image-size-adjust (1.0 . 1.0)
       :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
       :latex-header ,(string-join '("\\documentclass[crop,varwidth=\\maxdimen]{standalone}"
                                     "\\usepackage[usenames]{color}"
                                     "\\usepackage{amsmath}"
                                     "\\usepackage{amssymb}"
                                     "\\usepackage{ctex}")
                                   "\n")
       :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))
     org-preview-latex-process-alist :key #'car))
  :custom
  (org-latex-compiler "xelatex")
  (org-latex-custom-lang-environments '((Chinese "")))
  (org-latex-pdf-process '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f" "%latex -shell-escape -interaction nonstopmode -output-directory %o %f" "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-latex-packages-alist
   '(("" "ctex" nil nil)
     ("" "svg" nil nil)))
  (org-latex-image-default-width nil)
  (org-latex-image-default-height ".2\\linewidth")
  :config
  (defun org-create-formula-image-with-auto-processing-type (fun &rest args)
    (when (string-match "[^[:ascii:]]" (car args))
      (setf (nth 4 args) 'imagemagick-xelatex))
    (apply fun args))
  (advice-add #'org-create-formula-image :around #'org-create-formula-image-with-auto-processing-type)
  (let ((article-class (cl-copy-list (assoc-string "article" (default-value 'org-latex-classes)))))
    (setf (car article-class) "article-ctex"
          (cadr article-class) "\\documentclass[11pt]{ctexart}")
    (push article-class (default-value 'org-latex-classes))))

(use-package ox-md
  :ensure nil
  :demand t
  :after ox)

(use-package ox-beamer
  :ensure nil
  :demand t
  :after ox)

(use-package ox-hugo
  :when (<= 27 emacs-major-version)
  :ensure t
  :demand t
  :after ox)

(use-package mpv
  :when (member 'org-media-note extra-features)
  :ensure t
  :defer t
  :custom (mpv-default-options '("--volume-max=300")))

(use-package org-media-note
  :when (member 'org-media-note extra-features)
  :quelpa (org-media-note :fetcher github :repo "yuchen-lea/org-media-note")
  :defer t
  :hook (org-mode . org-media-note-mode)
  :bind(:map org-mode-map
        ("C-c v" . org-media-note-hydra/body))
  :custom
  (org-media-note-display-inline-images nil)
  (org-media-note-screenshot-image-dir (expand-file-name "org-media-note" org-directory)))

(use-package org-tree-slide
  :when (member 'org-tree-slide extra-features)
  :ensure t
  :after org
  :defer t
  :custom (org-tree-slide-slide-in-effect nil)
  :bind (:map org-mode-map
         ("<f5> <f5>" . org-tree-slide-mode)
         :map org-tree-slide-mode-map
         ("<prior>" . org-tree-slide-move-previous-tree)
         ("<next>" . org-tree-slide-move-next-tree)
         ("<f5> t" . org-tree-slide-skip-done-toggle)
         ("<f5> ;" . org-tree-slide-skip-comments-toggle)
         ("<f5> T" . org-tree-slide-display-header-toggle)
         ("<f5> a" . org-tree-slide-slide-in-effect-toggle)))

(use-package org-tree-slide-ext
  :when (member 'org-tree-slide extra-features)
  :load-path "custom-lisp"
  :after org-tree-slide
  :demand t
  :custom
  (org-tree-slide-header 'breadcrumbs-only)
  (org-tree-slide-line-spacing 10)
  (org-tree-slide-text-scale 4))

(use-package org-englearn
  :when (member 'org-englearn extra-features)
  :quelpa (org-englearn :fetcher github :repo "bohonghuang/org-englearn")
  :defer t
  :bind
  (("C-c e c" . org-englearn-capture)
   ("C-c e p" . org-englearn-process-inbox)
   :map org-capture-mode-map
   ("C-c e c" . org-englearn-capture-process-region)))

(use-package org-englearn-pdf-view
  :when (and (member 'org-englearn extra-features) (member 'pdf-tools extra-features))
  :ensure org-englearn
  :defer t
  :after pdf-view
  :custom (org-englearn-pdf-view-disable-org-pdftools-link t)
  :bind
  (:map pdf-view-mode-map
        ("C-c e c" . org-englearn-capture-pdf-view)))

;;;;;;;;;;;;
;; AUCTeX ;;
;;;;;;;;;;;;

(use-package cdlatex
  :when (member 'tex language-support-languages)
  :ensure t
  :defer t
  :init
  (use-package org
    :defer t
    :hook (org-mode . org-cdlatex-mode))
  :hook (latex-mode . turn-on-cdlatex)
  :custom
  (cdlatex-paired-parens "$[{(")
  (cdlatex-env-alist '(("cases" "\\begin{cases}\n?\n\\end{cases}" nil)
                       ("aligned" "\\begin{aligned}\n?\n\\end{aligned}" nil)))
  :config
  (use-package yasnippet
    :config
    (defun cdlatex-yas-expand-from-trigger-key (fun &rest args)
      (if cdlatex-mode
          (cdlatex-tab)
        (apply fun args)))
    (advice-add #'yas-expand-from-trigger-key :around #'cdlatex-yas-expand-from-trigger-key))
  (use-package cape
    :config
    (defun cape-dabbrev-disable-in-cdlatex-mode (ret)
      (unless cdlatex-mode ret))
    (advice-add #'cape-dabbrev :filter-return #'cape-dabbrev-disable-in-cdlatex-mode)))

(use-package tex
  :when (and (member 'tex language-support-languages) (member 'auctex extra-features))
  :ensure auctex
  :defer t
  :custom
  (TeX-engine 'xetex)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (TeX-electric-math nil); '("\\(" "\\)"));'("$" . "$"))
  (LaTeX-math-list '((nil "operatorname" "Log-like")
                     (nil "mathcal" "Log-like")
                     (nil "mathfrak" "Log-like")
                     (nil "mathbb" "Log-like")
                     (nil "mathnormal" "Log-like")
                     (nil "mathrm" "Log-like")
                     (nil "mathit" "Log-like")
                     (nil "mathbf" "Log-like")
                     (nil "mathsf" "Log-like")
                     (nil "mathtt" "Log-like")
                     (nil "dots"   "Log-like")
                     (nil "stackrel"   "Log-like")))
  :config
  (when (boundp 'tex-mode-hook)
    (dolist (hook tex-mode-hook) (cl-pushnew hook TeX-mode-hook))))

(use-package latex
  :when (and (member 'tex language-support-languages) (member 'auctex extra-features))
  :ensure auctex
  :defer t
  :config
  (when (boundp 'latex-mode-hook)
    (dolist (hook latex-mode-hook) (cl-pushnew hook LaTeX-mode-hook))))

(use-package go-translate
  :when (<= 27 emacs-major-version)
  :ensure t
  :defer t
  :bind (("C-c t t" . gt-do-translate))
  :config
  (setf (alist-get 'direction (cdr gt-buffer-render-window-config)) 'bottom
        (alist-get 'window-height (cdr gt-buffer-render-window-config)) (/ 3.0))
  (defcustom gt-default-taker (make-instance
                               'gt-taker
                               :text gt-taker-text
                               :pick gt-taker-pick
                               :langs '(en zh)
                               :then (lambda (translator)
                                       (with-slots (text) translator
                                         (setf text (mapcar (apply-partially #'replace-regexp-in-string "[[:space:]\n]+" " ") text)))))
    "The default taker used by `go-translate'.")
  (unless gt-default-translator
    (setf gt-default-translator (make-instance
                                 'gt-translator
                                 :taker gt-default-taker
                                 :engines (make-instance 'gt-youdao-dict-engine)
                                 :render (make-instance 'gt-buffer-render)))))

;;;;;;;;;;;;
;; Eshell ;;
;;;;;;;;;;;;

(use-package eshell
  :when (member 'eshell extra-features)
  :ensure nil
  :defer t
  :custom
  (eshell-highlight-prompt nil)
  (eshell-banner-message "")
  :config
  (defun eshell--complete-commands-list@around (fun &rest _)
    "Fix executable file completion for `eshell--complete-commands-list'"
    (if (looking-back "/[[:graph:]]*" (point-min))
        (pcomplete-executables)
      (funcall fun)))
  (advice-add #'eshell--complete-commands-list :around #'eshell--complete-commands-list@around))

(use-package esh-mode
  :when (member 'eshell extra-features)
  :ensure nil
  :defer t
  :config
  (cl-pushnew #'eshell-truncate-buffer eshell-output-filter-functions))

(use-package em-hist
  :when (member 'eshell extra-features)
  :ensure nil
  :defer t
  :custom
  (eshell-history-size 1000)
  (eshell-hist-ignoredups 'erase)
  :config
  (defun eshell-hist-write-after-command (&rest _)
    (when eshell-hist-mode
      (with-suppressed-message
        (eshell-read-history)
        (let ((input (buffer-substring-no-properties
                      eshell-last-input-start (1- eshell-last-input-end)))
              index
              earliest)
          (while (when (and (setq index (ring-member eshell-history-ring input)) (eq eshell-hist-ignoredups 'erase))
                   (ring-remove eshell-history-ring index)))
          (when (>= (ring-length eshell-history-ring) (ring-size eshell-history-ring))
            (setq earliest (ring-ref eshell-history-ring (1- (ring-length eshell-history-ring))))
            (with-temp-buffer
              (insert earliest)
              (newline)
              (write-region (point-min) (point-max) (concat eshell-history-file-name "_archive") 'append)))
          (unless (and index eshell-hist-ignoredups (not (eq eshell-hist-ignoredups 'erase)))
            (let ((eshell-hist-ignoredups nil))
              (eshell-add-input-to-history (string-trim input)))))
        (eshell-write-history))))
  (add-hook 'eshell-input-filter-functions #'eshell-hist-write-after-command)
  (defun eshell-hist-initialize@after (&rest _)
    (remove-hook 'eshell-input-filter-functions #'eshell-add-to-history t)
    (remove-hook 'eshell-exit-hook #'eshell-write-history t)
    (remove-hook 'kill-emacs-query-functions #'eshell-save-some-history))
  (advice-add #'eshell-hist-initialize :after #'eshell-hist-initialize@after))

(use-package em-term
  :when (member 'eshell extra-features)
  :ensure nil
  :defer t
  :custom
  (eshell-destroy-buffer-when-process-dies t)
  :config
  (nconc eshell-visual-commands '("nvtop" "bashtop" "btop" "vim" "nvim" "cmatrix" "ssh")))

(use-package eshell-syntax-highlighting
  :when (member 'eshell extra-features)
  :ensure t
  :defer t
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eshell-outline
  :when (member 'eshell extra-features)
  :ensure t
  :defer t
  :hook (eshell-mode . eshell-outline-mode)
  :config
  (setf (cdr eshell-outline-mode-map) nil))


(use-package eshell-prompt-extras
  :when (member 'eshell extra-features)
  :ensure t
  :after eshell
  :demand t
  :custom
  (eshell-prompt-function 'epe-theme-pipeline)
  (eshell-highlight-prompt t)
  :config
  (advice-add #'epe-remote-host :filter-return #'concat)
  (advice-add #'epe-remote-user :filter-return #'concat))

(use-package esh-autosuggest
  :when (member 'eshell extra-features)
  :ensure t
  :defer t
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package fish-completion
  :when (and (member 'eshell extra-features) (executable-find "fish"))
  :ensure t
  :defer t
  :hook (eshell-mode . fish-completion-mode))

(use-package vterm
  :when (member 'vterm extra-features)
  :ensure t
  :defer t
  :bind (:map vterm-mode-map
         ("C-x C-q" . vterm-copy-mode)
         :map vterm-copy-mode-map
         ("C-x C-q" . vterm-copy-mode)))

(use-package eat
  :when (<= 28 emacs-major-version)
  :unless (member 'vterm extra-features)
  :commands (eat-other-window)
  :ensure t
  :defer t
  :hook
  (eat-mode . toggle-truncate-lines)
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)
  (eshell-load . (lambda () (setf eshell-visual-commands nil)))
  :config
  (defun eat-other-window (&optional program args)
    (interactive)
    (switch-to-buffer-other-window
     (let ((buffer (save-window-excursion
                     (if program (eat program args) (call-interactively #'eat)))))
       (with-current-buffer buffer
         (set (make-local-variable 'eat-kill-buffer-on-exit) t)
         (add-hook 'kill-buffer-hook #'quit-window nil t))
       buffer))))

(use-package eshell-vterm
  :when (and (<= 27 emacs-major-version) (member 'vterm extra-features))
  :ensure t
  :demand t
  :after eshell
  :config
  (eshell-vterm-mode +1))

(use-package quickrun
  :when (<= 26 emacs-major-version)
  :ensure t
  :defer t
  :init (defalias 'qr 'quickrun)
  :custom (quickrun-timeout-seconds -1))

(use-package command-log-mode
  :ensure t
  :defer t)

(use-package elmacro
  :ensure t
  :defer t)

(use-package mermaid-mode
  :when (member 'mermaid language-support-languages)
  :ensure t
  :defer t)

(use-package ob-mermaid
  :when (member 'mermaid language-support-languages)
  :ensure t
  :defer t)

(use-package rime
  :when (member 'rime extra-features)
  :ensure t
  :defer t
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  :config
  (dolist (it '("C-a" "C-e" "C-k" "C-v" "M-v" "TAB"))
    (cl-pushnew it rime-translate-keybindings)))

(use-package redacted
  :ensure t
  :defer t
  :bind (("<pause>" . redacted-mode)))

(use-package emms
  :when (member 'emms extra-features)
  :ensure t
  :defer t
  :hook (emms-player-started . emms-show)
  :commands emms
  :custom
  (emms-playlist-buffer-name "*Music*")
  (emms-info-asynchronously t)
  (emms-lyrics-scroll-p nil)
  :bind (("C-c m" . hydra-emms/body))
  :config
  (emms-all)
  (unless emms-player-list
    (emms-default-players))
  (emms-mode-line-disable))

(use-package emms-mark
  :when (member 'emms extra-features)
  :ensure emms
  :defer t
  :bind (:map emms-mark-mode-map
         ("n" . next-line)
         ("p" . previous-line)))

(use-package emms-ext
  :when (member 'emms extra-features)
  :load-path "custom-lisp"
  :demand t
  :after emms
  :commands hydra-emms/body)

(use-package emms-vgm
  :when (member 'emms extra-features)
  :quelpa (emms-vgm :fetcher github :repo "bohonghuang/emms-vgm")
  :demand t
  :after emms
  :config
  (use-package emms-vgm-default-players
    :ensure emms-vgm
    :demand t
    :config
    (setq emms-player-list (append emms-vgm-default-players emms-player-list))))

(use-package consult-emms
  :when (and (<= 27 emacs-major-version) (member 'emms extra-features))
  :quelpa (consult-emms :fetcher github :repo "Hugo-Heagren/consult-emms")
  :defer t)

(use-package mu4e
  :when (member 'mu4e extra-features)
  :defer t
  :custom
  (mu4e-get-mail-command       "mbsync -a")
  (mail-user-agent             'mu4e-user-agent)
  (message-send-mail-function  'message-send-mail-with-sendmail)
  (sendmail-program            (executable-find "msmtp"))
  :config
  (dolist (file (directory-files
                 (expand-file-name "mu4e/accounts" (or (getenv "XDG_CONFIG_HOME") "~/.config")) t "\\.el$" nil))
    (load file)))

(use-package edit-server
  :defer t
  :ensure t
  :commands edit-server-start
  :init (when (daemonp)
          (if after-init-time
              (edit-server-start)
            (add-hook 'after-init-hook
                      #'(lambda () (edit-server-start)))))
  :custom (edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (top . 200)
                  (left . 200)
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . t))))

(use-package htmlize
  :ensure t
  :defer t)

(use-package frameshot
  :when (<= 26 emacs-major-version)
  :ensure t
  :defer t
  :commands frameshot-temp
  :config
  (defun frameshot-temp (&optional type)
    "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
    (interactive)
    (let*((type (or type 'svg))
          (filename (make-temp-file "Emacs_" nil (format ".%s" type)))
          (data (x-export-frames nil type)))
      (with-temp-file filename
        (insert data))
      (kill-new filename)
      (message (format "Frameshot saved: %s" filename)))))

(use-package xwidget
  :ensure nil
  :defer t
  :bind (:map xwidget-webkit-edit-mode-map ("C-c '" . xwidget-webkit-insert-string))
  :config
  (defun xwidget-webkit-begin-edit-textarea-bind-key (&rest _)
    (local-set-key (kbd "C-c C-c") #'xwidget-webkit-end-edit-textarea)
    (local-set-key (kbd "C-c '") #'xwidget-webkit-end-edit-textarea))
  (advice-add #'xwidget-webkit-begin-edit-textarea :after #'xwidget-webkit-begin-edit-textarea-bind-key)
  (advice-add #'xwidget-webkit-end-edit-textarea :after #'kill-current-buffer))

(use-package app-launcher
  :when (<= 27 emacs-major-version)
  :defer t
  :quelpa (app-launcher :fetcher github :repo "bohonghuang/app-launcher")
  :bind (("s-SPC" . app-launcher-run-app))
  :commands (app-launcher-make-frame-and-run-app)
  :config
  (defun app-launcher-make-frame-and-run-app ()
    (interactive)
    (with-selected-frame (make-frame '((name . "Run Application")
                                       (minibuffer . only)
                                       (width . 120)
                                       (height . 11)
                                       (window-system . pgtk)
                                       (undecorated . t)))
      (unwind-protect
          (let ((enable-recursive-minibuffers nil)) (app-launcher-run-app))
        (delete-frame)))))

(defcustom sis-ism-lazyman-config nil
  "Out-of-the-box configuration used by SIS.")

(use-package sis
  :when (member 'sis extra-features)
  :ensure t
  :defer nil
  :config
  (cl-case sis-ism-lazyman-config
    (fcitx5 (sis-ism-lazyman-config "1" "2" 'fcitx5))
    (ibus (sis-ism-lazyman-config "xkb:us::eng" "rime" 'ibus)))
  (sis-global-cursor-color-mode +1)
  (sis-global-respect-mode +1)
  (sis-global-context-mode +1)
  (defvar sis-global-respect-mode-before-kbd-macro nil)
  (defvar sis-global-respect-mode-previous-defining-kbd-macro nil)
  (defun sis-defining-kbd-macro-watcher (&rest _)
    (unless (eq sis-global-respect-mode-previous-defining-kbd-macro defining-kbd-macro)
      (if defining-kbd-macro
          (when sis-global-respect-mode
            (setq sis-global-respect-mode-before-kbd-macro sis-global-respect-mode)
            (sis-global-respect-mode -1))
        (when sis-global-respect-mode-before-kbd-macro
          (setq sis-global-respect-mode-before-kbd-macro nil)
          (sis-global-respect-mode +1)))
      (setq sis-global-respect-mode-previous-defining-kbd-macro defining-kbd-macro)))
  (add-hook 'post-command-hook #'sis-defining-kbd-macro-watcher)
  (when (featurep 'native-compile) (native-compile #'sis-defining-kbd-macro-watcher))
  (defun sis-update-cursor-color-after-theme-load (&rest _)
    (sis-global-cursor-color-mode -1)
    (let ((cursor-color (face-background 'cursor)))
      (add-hook 'post-command-hook
                (defun sis-update-default-cursor-color ()
                  (setq sis-default-cursor-color cursor-color)
                  (sis-global-cursor-color-mode +1)
                  (remove-hook 'post-command-hook #'sis-update-default-cursor-color)
                  (fmakunbound #'sis-update-default-cursor-color)))))
  (advice-add #'consult-theme :after #'sis-update-cursor-color-after-theme-load)
  (sis--update-cursor-color))

(provide 'init)
;;; init.el ends here
