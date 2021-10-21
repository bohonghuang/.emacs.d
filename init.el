;;; Package -- Summary 

(progn
  (setq original-gc-cons-threshold gc-cons-threshold)
  (setq gc-cons-threshold 50000000)
  (add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold original-gc-cons-threshold))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file) (load-file custom-file))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package use-package
  :defer nil
  :ensure nil
  :custom
  (use-package-verbose t)
  (use-package-minimum-reported-time 0.01))

(use-package comp
  :ensure nil
  :defer nil
  :custom (package-native-compile t))

(use-package startup
  :ensure nil
  :defer nil
  :custom
  (inhibit-startup-screen t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message ""))

(use-package simple
  :ensure nil
  :defer nil
  :hook (prog-mode . toggle-word-wrap)
  :bind (("C-?" . undo-redo)
         ("S-<backspace>" . delete-indentation))
  :custom
  (column-number-mode t)
  (visible-bell t)
  (show-paren-mode t)
  (indent-tabs-mode nil))

(use-package subr
  :ensure nil
  :defer t
  :init (defalias 'yes-or-no-p 'y-or-n-p))

(use-package files
  :ensure nil
  :defer t
  :custom (make-backup-files nil))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(defun change-theme ()
  "Disable all themes and then load a single theme interactively."
  (interactive)
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (call-interactively 'load-theme))

(use-package monokai-theme
  :load-path "site-lisp/monokai-theme"
  :config
  (load-theme 'monokai t))

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(setq local-file (expand-file-name "local.el" user-emacs-directory))
(if (file-exists-p local-file) (load-file local-file))

(use-package hl-line
  :ensure nil
  :defer nil
  :hook (prog-mode . hl-line-mode))

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

(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-dwim-target t)
  (browse-url-handlers '(("\\`file:" . browse-url-default-browser)))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package savehist
  :ensure nil
  :defer t
  :config
  (savehist-mode t))

(use-package display-line-numbers
  :ensure nil
  :defer t
  :hook (prog-mode . display-line-numbers-mode)
  :custom (display-line-numbers-type t))

(use-package xref
  :ensure nil
  :defer t
  :bind (("<mouse-8>" . xref-pop-marker-stack)))

(use-package compile
  :ensure nil
  :defer t
  :custom
  (compilation-scroll-output t))

(use-package winner
  :ensure nil
  :defer t
  :config
  (winner-mode +1))

(use-package recentf
  :init (defalias 'reopfs 'recentf-open-files)
  :defer t
  :hook (find-file . (lambda () (require 'recentf)))
  :commands recentf-open-files
  :custom (recentf-exclude '("~$" "/tmp/" "/ssh:" "/sshx:" "/sudo:"))
  :config
  (recentf-mode +1)
  (let ((file-name (buffer-file-name)))
    (if (and file-name (file-exists-p file-name))
      (recentf-add-file buffer-file-name))))

(global-set-key (kbd "C-x O") (lambda ()
                          (interactive)
                          (other-window -1)))

(use-package ace-window
  :ensure t
  :defer t
  :bind (("M-o" . ace-window))
  :custom (aw-dispatch-always t))

(use-package doom-modeline
  :ensure t
  :defer nil
  :config
  (doom-modeline-mode +1))

(use-package crux
  :ensure t
  :defer t
  :bind (:map global-map
         ("C-x C-S-e" . crux-eval-and-replace)
         ("C-x C-S-f" . crux-open-with)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-<return>" . crux-smart-open-line)
         ("S-<f1>" . crux-find-user-init-file)))

(add-hook 'find-file-hook (lambda ()
                                               (unless (file-exists-p (file-truename buffer-file-name))
                                                 (set-buffer-file-coding-system 'utf-8))))

(use-package cnfonts
  :ensure t
  :defer t
  :bind (("C-M-_" . cnfonts-decrease-fontsize)
         ("C-M-+" . cnfonts-increase-fontsize)
         ("C-M-)" . cnfonts-reset-fontsize))
  :custom
  (cnfonts-personal-fontnames '(("JetBrains Mono") nil nil))
  (cnfonts-use-face-font-rescale t)
  (cnfonts-use-cache t)
  :config
  (advice-add #'cnfonts--step-fontsize :after (lambda (&rest _) (and (boundp 'doom-modeline-mode) doom-modeline-mode (doom-modeline-refresh-font-width-cache)))))

(use-package ligature
  :defer t
  :hook (prog-mode . (lambda () (unless (-contains-p '(emacs-lisp-mode lisp-mode) major-mode) (require 'ligature) (ligature-mode +1))))
  :load-path "site-lisp/"
  :config
  ;; Enable all JetBrains Mono ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                      "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                      "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                      "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                      "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                      "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                      ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                      "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                      "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                      "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                      "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#")))

(defun dedicate-window ()
  (interactive)
  (set-window-dedicated-p (selected-window) t))

(defalias 'window-buffer-change-hook 'window-buffer-change-functions)

(use-package popper
  :defer nil
  :ensure t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers '("\\*Messages\\*"
                              "Output\\*$"
                              "\\*Async Shell Command\\*"
                              "\\*rustic-compilation\\*"
                              eshell-mode
                              vterm-mode
                              help-mode
                              compilation-mode
                              dap-server-log-mode
                              xref--xref-buffer-mode
                              quickrun--mode
                              flymake-diagnostics-buffer-mode))
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package smartparens
  :ensure t
  :defer t
  :hook
  ((prog-mode text-mode minibuffer-mode) . smartparens-mode)
  :bind (:map smartparens-mode-map
              ("C-*" . sp-join-sexp)
              ("C-|" . sp-split-sexp)
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-S-d" . sp-backward-down-sexp)
              ("C-S-a" . sp-beginning-of-sexp)
              ("C-S-e" . sp-end-of-sexp)
              ("C-M-u" . sp-up-sexp)
              ("C-M-S-u" . sp-backward-up-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-M-\"" . sp-backward-unwrap-sexp)
              ("C-\"" . sp-unwrap-sexp)
              ("M-\"" . sp-rewrap-sexp)
              ("C-M-\"" . sp-splice-sexp)
              ("C-)" . sp-select-next-thing-exchange)
              ("C-M-)" . sp-select-next-thing)
              ("C-(" . sp-select-previous-thing-exchange)
              ("C-M-(" . sp-select-previous-thing)
              ("C-M-SPC" . sp-mark-sexp)
          :map emacs-lisp-mode-map
              ("M-<right>" . sp-forward-slurp-sexp)
              ("M-<left>" . sp-forward-barf-sexp)
              ("C-M-t" . sp-transpose-sexp)
              ("C-M-T" . sp-convolute-sexp)
              ("C-M-<left>" . sp-backward-slurp-sexp)
              ("C-M-<right>" . sp-backward-barf-sexp)
              ("C-M-<delete>" . sp-splice-sexp-killing-forward)
              ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
              ("C-M-S-<backspace>" . sp-splice-sexp-killing-around)
              ("M-F" . sp-forward-symbol)
              ("M-B" . sp-backward-symbol))
  :init
  (require 'smartparens-config)
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook
  (lisp-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode))

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

(use-package repeat
  :if (>= emacs-major-version 28)
  :ensure nil
  :defer nil
  :config
  (repeat-mode +1))

(use-package good-scroll
  :ensure t
  :defer nil
  :custom (good-scroll-step 100)
  :config
  (good-scroll-mode +1))

(use-package projectile
  :ensure t
  :defer t
  :hook
  (find-file . projectile-mode)
  (dired-mode . (lambda () (require 'projectile)))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
  (put 'compilation-read-command 'safe-local-variable #'booleanp))

(use-package elisp-mode
  :ensure nil
  :defer t
  :init (defalias 'elisp-mode 'emacs-lisp-mode))

;; ======================================== Scala ========================================

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :ensure t
  :defer t
  :mode ("\\.sc\\'" . scala-mode)
  :interpreter
  ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :ensure t
  :defer t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

(use-package flymake
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . flymake-mode)
  :bind
  ("C-c f f" . flymake-show-buffer-diagnostics)
  ("C-c f p" . flymake-show-project-diagnostics)
  ("C-c f C-n" . flymake-goto-next-error)
  ("C-c f C-p" . flymake-goto-prev-error)
  :config
  (defvar flymake-navigation-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-n") #'flymake-goto-next-error)
      (define-key map (kbd "C-p") #'flymake-goto-prev-error)
      (--each '(flymake-goto-next-error
                flymake-goto-prev-error)
        (put it 'repeat-map 'flymake-navigation-repeat-map))
      map)
    "Keymap to repeat flymake navigation key sequences.  Used in `repeat-mode'."))

(use-package subword
  :ensure nil
  :defer t
  :hook ((c++-mode java-mode scala-mode) . subword-mode))

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

(use-package tree-sitter
  :ensure t
  :defer t
  :hook (prog-mode . (lambda ()
                       (require 'tree-sitter-langs)
                       (if (assoc major-mode tree-sitter-major-mode-language-alist)
                           (tree-sitter-mode +1))))
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :defer t)

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :ensure t
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (lsp-mode . lsp-lens-mode)
  :custom
  (lsp-eldoc-hook nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-completion-provider :capf)
  (read-process-output-max (* 1024 1024 16)) ;; 1mb
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.5)
  (lsp-idle-delay 0.5)
  (lsp-log-io nil))

(use-package lsp-metals
  :ensure t
  :defer t
  :custom
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off -Xmx8192m"))
  :hook (scala-mode . lsp)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "metals")
                    :major-modes '(scala-mode)
                    :remote? t
                    :server-id 'metals-remote)))

(use-package lsp-ui
  :ensure t
  :defer t
  :after lsp)

(use-package company
  :ensure t
  :defer t
  :hook
  (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-frontends '(company-pseudo-tooltip-frontend
                          company-echo-metadata-frontend))
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t))

(use-package posframe
  :ensure t
  :defer t)

(use-package dap-mode
  :ensure t
  :defer t
  :bind(:map prog-mode-map
        ("C-c l d" . dap-debug)
        ("C-<f8>". dap-breakpoint-toggle)
        ("<f8>" . dap-continue)
        ("S-<f8>" . dap-step-out)
        ("<f7>" . dap-step-in)
        ("C-<f2>" . dap-disconnect)
        ("C-S-<f2>" . dap-stop-thread))
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

;; ================================================================================

;; =======================================Rust =========================================

(use-package rustic
  :ensure t
  :defer t
  :config
  (require 'lsp)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "rust-analyzer")
                    :major-modes '(rustic-mode)
                    :remote? t
                    :server-id 'rust-analyzer-remote)))

;; ======================================== Groovy ========================================

(use-package groovy-mode
  :ensure t
  :defer t)


;; ====================================== Python ==========================================
(use-package python
  :ensure nil
  :defer t
  :config
  (defvar python-indent-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<") #'python-indent-shift-left)
      (define-key map (kbd ">") #'python-indent-shift-right)
      (--each '(python-indent-shift-left python-indent-shift-right) (put it 'repeat-map 'python-indent-repeat-map))
      map)
    "Keymap to repeat Python indentation key sequences.  Used in `repeat-mode'."))

(use-package lsp-pyright
  :ensure t
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-tramp-connection "pyright")
  ;;                   :major-modes '(python-mode)
  ;;                   :remote? t
  ;;                   :server-id 'pyright-remote))
  (require 'dap-python)
  (dap-register-debug-template "Python Program"
                               (list :type "python"
                                     :args "-i"
                                     :cwd nil
                                     :env '(("DEBUG" . "1"))
                                     :target-module (expand-file-name "~/工程/Python")
                                     :request "launch"
                                     :name "Python Program")))  ; or lsp-deferred

(use-package ein
  :ensure t
  :defer t)

;; ================================================================================

;; ======================================== C++ ========================================

;; Clangd

(use-package which-key
  :ensure t
  :config (which-key-mode t))

;;

(use-package lsp-clangd
  :defer t
  :hook
  ((c-mode c++-mode objc-mode) . lsp)
  :custom
  (lsp-clients-clangd-args '("--header-insertion=never"))
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode c++-mode objc-mode)
                    :remote? t
                    :server-id 'clangd-remote)))

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package qml-mode
  :ensure t
  :defer t)

;; ================================================================================


(use-package lsp-java
  :ensure t
  :defer t
  :hook (java-mode . lsp)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "java-language-server")
                    :major-modes '(java-mode)
                    :remote? t
                    :server-id 'lsp-java-remote)))

;; ======================================== PlatformIO ========================================

(use-package platformio-mode
  :ensure t
  :defer t)

;; ================================================================================

(use-package scad-mode
  :ensure t
  :defer t)

(use-package scad-preview
  :ensure t
  :defer t
  :bind(:map scad-mode-map
             ("C-c C-c" . scad-preview-mode)))

;; ======================================= VHDL =======================================

(use-package lsp-vhdl
  :load-path "site-lisp"
  :defer nil
  :hook
  (vhdl-mode . lsp)
  (vhdl-mode . (lambda () (ligature-mode -1)))
  :config
  (setq lsp-vhdl-server 'ghdl-ls))

(use-package vhdl-capf
  :ensure t
  :defer t
  :hook (vhdl-mode . vhdl-capf-enable)
  :config
  (defun vhdl-capf-flatten (l) (-flatten l)))

;; ====================================================================================

(use-package markdown-mode
  :ensure nil
  :defer t
  :init
  (defalias 'md-mode 'markdown-mode))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package toml-mode
  :ensure t
  :defer t)

(use-package magit
  :defer t
  :ensure t)

(use-package yasnippet
  :ensure t
  :defer t
  :hook
  ((prog-mode org-mode latex-mode) . yas-minor-mode)
  :custom
  (yas-triggers-in-field t)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :hook (yas-minor-mode . (lambda () (require 'yasnippet-snippets)))) 

(use-package binary-jump
  :load-path "custom-lisp"
  :defer t
  :bind (("M-P" . binary-jump-previous-line)
         ("M-N" . binary-jump-next-line)
         ("M-J" . binary-jump-select-line-command)))

(use-package expand-region
  :ensure t
  :defer t
  :bind (( "C-=" . er/expand-region)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" . mc/skip-to-previous-like-this)))

(use-package intellij-features
  :load-path "custom-lisp"
  :defer t
  :hook
  ((c-mode
    c++-mode
    objc-mode
    java-mode
    scala-mode
    rustic-mode
    js-mode)
   .
   (lambda ()
     (require 'intellij-features)
     (local-set-key (kbd "DEL") #'intellij-backspace)
     (local-set-key (kbd "{") #'intellij-left-bracket)
     (when (-contains-p '(java-mode rustic-mode js-mode) major-mode)
       (local-set-key (kbd "RET") #'intellij-return))))
  (python-mode . (lambda ()
                   (require 'intellij-features)
                   (local-set-key (kbd "RET") #'pycharm-return)
                   (local-set-key (kbd "DEL") #'pycharm-backspace))))

(use-package sis
  :ensure t
  :defer t
  :config
  ;; For MacOS
  (sis-ism-lazyman-config "1" "2" 'fcitx5)
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t))

(use-package calendar
  :ensure nil
  :defer t
  :custom
  (calendar-mark-today t)
  (calendar-chinese-all-holidays-flag t))

(use-package org
  :ensure nil
  :defer t
  :custom
  (org-adapt-indentation t)
  (org-attach-use-inheritance t)
  (org-export-with-tags nil)
  (org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (org-attach-id-dir (expand-file-name "org-attach/data" org-directory))

  (org-default-notes-file (expand-file-name "org-capture/captures.org" org-directory))
  (org-latex-compiler "xelatex")
  (org-latex-custom-lang-environments '((Chinese "")))
  (org-latex-pdf-process '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f" "%latex -shell-escape -interaction nonstopmode -output-directory %o %f" "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-latex-default-packages-alist
   '(("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t nil)
     ("" "grffile" t nil)
     ("" "longtable" nil nil)
     ("" "wrapfig" nil nil)
     ("" "rotating" nil nil)
     ("normalem" "ulem" t nil)
     ("" "amsmath" t nil)
     ("" "textcomp" t nil)
     ("" "amssymb" t nil)
     ("" "capt-of" nil nil)
     ("" "hyperref" nil nil)
     ("a4paper,left=2cm,right=2cm,top=2cm,bottom=2cm" "geometry" nil nil)
     ("" "ctex" nil nil)
     ("" "svg" nil nil)))
  (org-latex-image-default-width nil)
  (org-latex-image-default-height "120pt"))

(use-package ob
  :ensure nil
  :defer t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t) (python . t))))

(use-package org-pomodoro
  :ensure t
  :defer t
  :bind (("<f12>" . org-pomodoro))
  :custom (org-pomodoro-keep-killed-pomodoro-time t)
  :hook (org-agenda-mode . (lambda () (require 'org-pomodoro))))

(use-package org-pomodoro-ext
  :load-path "custom-lisp"
  :defer t
  :bind (("S-<f12>" . org-pomodoro-interrupt))
  :hook (org-pomodoro-started . (lambda () (require 'org-pomodoro-ext))))

(use-package org-ext
  :defer t
  :load-path "custom-lisp"
  :hook (org-mode . (lambda () (require 'org-ext)))
  :bind (:map org-mode-map
         ("C-c C-S-L" . org-link-make-from-region)))

(use-package org-gtd
  :ensure t
  :defer t
  :hook (org-agenda-mode . (lambda () (require 'org-gtd)))
  ;; :demand t ;; without this, the package won't be loaded, so org-agenda won't be configured
  :custom
  ;; where org-gtd will put its files. This value is also the default one.
  (org-gtd-directory (expand-file-name "org-gtd" org-directory))
  ;; package: https://github.com/Malabarba/org-agenda-property
  ;; this is so you can see who an item was delegated to in the agenda
  (org-agenda-property-list '("DELEGATED_TO"))
  ;; I think this makes the agenda easier to read
  (org-agenda-property-position 'next-line)
  ;; package: https://www.nongnu.org/org-edna-el/
  ;; org-edna is used to make sure that when a project task gets DONE,
  ;; the next TODO is automatically changed to NEXT.
  (org-edna-use-inheritance t)
  :config
  (org-edna-load)
  :bind
  (("C-c g c" . org-gtd-capture) ;; add item to inbox
   ("C-c g a" . org-agenda-list) ;; see what's on your plate today
   ("C-c g p" . org-gtd-process-inbox) ;; process entire inbox
   ("C-c g n" . org-gtd-show-all-next) ;; see all NEXT items
   ("C-c g s" . org-gtd-show-stuck-projects)) ;; see projects that don't have a NEXT item
  :init
  (bind-key "C-c g g" 'org-gtd-clarify-finalize)) ;; the keybinding to hit when you're done editing an item in the processing phase

(use-package org-agenda
  :ensure nil ;; this is how you tell use-package to manage a sub-package
  :after org-gtd ;; because we need to add the org-gtd directory to the agenda files
  :bind (:map org-mode-map
              ("C-c C-x C-S-o" . org-resolve-clocks)
              ("<mouse-8>" . org-mark-ring-goto)
         :map org-agenda-mode-map
              ("C-c C-x C-S-o" . org-resolve-clocks))
  :custom
  ;; use as-is if you don't have an existing org-agenda setup
  ;; otherwise push the directory to the existing list
  (org-agenda-files (list (expand-file-name "org-agenda" org-directory) org-gtd-directory))
  ;; (org-agenda-files `(,org-gtd-directory))
  ;; a useful view to see what can be accomplished today
  (org-agenda-custom-commands '(("g" "Scheduled today and all NEXT items" ((agenda "" ((org-agenda-span 1))) (todo "NEXT")))))
  :config
  (require 'recentf)
  (nconc recentf-exclude (org-agenda-files)))


(use-package org-capture
  :ensure nil
  ;; note that org-gtd has to be loaded before this
  :after org-gtd
  :custom (org-capture-templates `(("i" "Inbox"
                                    entry (file ,(org-gtd--path org-gtd-inbox-file-basename))
                                    "* %?\n%U\n\n  %i"
                                    :kill-buffer t)
                                   ("l" "Todo with link"
                                    entry (file ,(org-gtd--path org-gtd-inbox-file-basename))
                                    "* %?\n%U\n\n  %i\n  %a"
                                    :kill-buffer t)))
  :config
  ;; use as-is if you don't have an existing set of org-capture templates
  ;; otherwise add to existing setup
  ;; you can of course change the letters, too
  )

(use-package org-download
  :defer t
  :ensure t
  :hook (org-mode . (lambda () (require 'org-download)))
  :custom
  (org-download-display-inline-images nil)
  (org-download-method 'attach))

(setq org-noter-default-notes-file-names '("notes.org")
      org-noter-notes-search-path '((expand-file-name "org-noter" org-directory)))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :ensure t
  :defer t
  :custom
  (org-roam-directory (expand-file-name "org-roam" org-directory))
  (org-roam-graph-link-hidden-types '("file" "attachment"))
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ;; Dailies
         ("C-c r j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  (require 'org-roam-protocol))

(use-package org-transclusion
  :load-path "site-lisp/org-transclusion"
  :defer t
  :bind (:map org-mode-map
         ("C-c t a" . org-transclusion-add)
         ("C-c t t" . org-transclusion-mode)))

(use-package org-journal
  :ensure t
  :defer t
  :init
  (setq org-journal-prefix-key "C-c j")
  :bind (("C-c j j" . org-journal-new-entry))
  :custom
  (org-journal-date-format "%Y年%m月%d日 %A")
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

(use-package htmlize
  :ensure t
  :defer t)

(use-package ox-md
  :ensure nil
  :defer t
  :hook (org-mode . (lambda () (require 'ox-md))))

(use-package ox-reveal
  :ensure t
  :defer t
  :hook (org-mode . (lambda () (require 'ox-reveal))))

(defun org-media-note-hydra/body-with-sis-set-english ()
  (interactive)
  (sis-set-english)
  (org-media-note-hydra/body))

(use-package pretty-hydra
  :ensure t
  :defer t)

(use-package mpv
  :ensure t
  :defer t)

(use-package org-media-note
  :load-path "site-lisp/org-media-note"
  :defer t
  :hook (org-mode . org-media-note-mode)
  :bind(:map org-mode-map
             ("C-c m" . org-media-note-hydra/body-with-sis-set-english))
  :custom
  (org-media-note-display-inline-images nil)
  (org-media-note-screenshot-image-dir (expand-file-name "org-media-note" org-directory)))

(use-package org-link-edit
  :load-path "site-lisp/org-link-edit"
  :defer t)

(use-package org-sketch
  :load-path "site-lisp/org-sketch"
  :defer t
  :hook (org-mode . org-sketch-mode)
  :bind(:map org-mode-map
             ("C-c s s" . org-sketch-insert))
  :custom
  (org-sketch-xournal-template-dir (expand-file-name "site-lisp/org-sketch/template" user-emacs-directory))  ;; xournal 模板存储目录
  (org-sketch-xournal-default-template-name "template.xopp") ;; 默认笔记模版名称，应该位于 org-sketch-xournal-template-dir
  (org-sketch-apps '("xournal" "drawio")))

(use-package org-tree-slide
  :ensure t
  :defer t
  :bind (:map org-mode-map
         ("<f8>" . org-tree-slide-mode)
         ("S-<f8>" . 'org-tree-slide-skip-done-toggle)
         :map org-tree-slide-mode-map
              ("<f9>" . org-tree-slide-move-previous-tree)
              ("<f10>" . org-tree-slide-move-next-tree)))

(use-package vterm
  :ensure t
  :defer t
  :bind (:map vterm-mode-map
              ("C-x C-q" . vterm-copy-mode)
         :map vterm-copy-mode-map
              ("C-x C-q" . vterm-copy-mode)))

(use-package quickrun
  :ensure t
  :defer t
  :init (defalias 'qr 'quickrun))

(use-package command-log-mode
  :ensure t
  :defer t)

(use-package elmacro
  :ensure t
  :defer t)

(use-package mermaid-mode
  :ensure t
  :defer t)

(use-package ob-mermaid
  :ensure t
  :defer t)

(use-package rime
  :ensure t
  :defer t
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  :config
  (--each '("C-v" "M-v" "S-<delete>") (add-to-list 'rime-translate-keybindings it)))

(use-package secret-mode
  :load-path "site-lisp/secret-mode"
  :defer t
  :commands secret-mode
  :bind (("<pause>" . secret-mode)))

(use-package emms
  :ensure t
  :defer t
  :commands emms
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-playlist-buffer-name "*Music*")
  (emms-info-asynchronously t)
  (emms-info-functions '(emms-info-libtag))
  :config
  (require 'emms-setup)
  (require 'emms-info-libtag)
  (emms-all)
  (emms-default-players)
  (emms-mode-line-disable)
  (emms-playing-time-disable-display))

(use-package edit-server
  :defer t
  :ensure t
  :commands edit-server-start
  :init (when (daemonp)
          (if after-init-time
              (edit-server-start)
            (add-hook 'after-init-hook
                      #'(lambda() (edit-server-start)))))
  :custom (edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (top . 200)
                  (left . 200)
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . t))))

(use-package bar-cursor
  :if (daemonp)
  :ensure t
  :config
  (bar-cursor-mode +1)
  (menu-bar-mode +1)
  (global-tab-line-mode +1)
  (scroll-bar-mode +1))

(when (not window-system)
  (global-set-key (kbd "M-=") 'er/expand-region))

(provide 'init)
;;;
