;;; Package -- Summary
(progn
  (setq original-gc-cons-threshold gc-cons-threshold)
  (setq gc-cons-threshold 50000000)
  (add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold original-gc-cons-threshold))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file) (load-file custom-file))

(custom-set-variables
 '(avy-single-candidate-jump nil)
 '(column-number-mode t)
 '(company-minimum-prefix-length 1)
 '(compilation-scroll-output t)
 '(display-line-numbers-type 'relative)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'fundamental-mode)
 '(initial-scratch-message "")
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(warning-suppress-log-types '((comp)))
 '(winner-mode t))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'visual-line-mode)
(global-set-key (kbd "C-?") 'undo-redo)
(global-set-key (kbd "C-S-d") 'delete-region)
;; (global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-x o") 'ace-window)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)
(defalias 'elisp-mode 'emacs-lisp-mode)
;; load emacs 24's package system. Add MELPA repository.
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

(setq use-package-verbose t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(defun change-theme ()
  "Disable all themes and then load a single theme interactively."
  (interactive)
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (call-interactively 'load-theme))

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-gruvbox t))

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(setq local-file (expand-file-name "local.el" user-emacs-directory))
(if (file-exists-p local-file) (load-file local-file))

(use-package recentf
  :init (defalias 'reopfs 'recentf-open-files)
  :defer t
  :hook (find-file . (lambda () (require 'recentf)))
  :commands recentf-open-files
  :config
  (recentf-mode +1)
  (let ((file-name (buffer-file-name)))
    (if (and file-name (file-exists-p file-name))
      (recentf-add-file buffer-file-name))))

(use-package crux
  :ensure t
  :defer t
  :bind (("C-x C-S-e" . crux-eval-and-replace)
         ("C-x C-S-f" . crux-open-with)))

(defun my-new-file-hook ()
  (unless (file-exists-p (file-truename buffer-file-name))
    (set-buffer-file-coding-system 'utf-8)))

(add-to-list 'find-file-hook #'my-new-file-hook)

(use-package cnfonts
  :ensure t
  :bind (("C-M-_" . cnfonts-decrease-fontsize)
         ("C-M-+" . cnfonts-increase-fontsize))
  :init
  (cnfonts-enable)
  :custom
  (cnfonts-personal-fontnames '(("JetBrains Mono") nil nil))
  (cnfonts-use-face-font-rescale t))

(use-package ligature
  :defer t
  :hook (prog-mode . ligature-mode)
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

(use-package smartparens
  :ensure t
  :defer t
  :hook
  (prog-mode . smartparens-mode)
  (text-mode . smartparens-mode)
  (minibuffer-mode . smartparens-mode)
  :bind (:map smartparens-mode-map
              ("C-(" . sp-unwrap-sexp))
  :init
  (require 'smartparens-config)
  :config
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook
  (lisp-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package good-scroll
  :ensure t
  :defer nil
  :hook (org-mode . (lambda ()
                      (local-set-key (kbd "C-v") (lambda () (interactive) (good-scroll-move (/ (good-scroll--window-usable-height) 2))))
                      (local-set-key (kbd "M-v") (lambda () (interactive) (good-scroll-move (- (/ (good-scroll--window-usable-height) 2)))))))
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

;; ======================================== Scala ========================================

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :ensure t
  :defer t
  :interpreter
  ("scala" . scala-mode)
  )

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
  :ensure nil
  :defer t
  :bind (("C-c f" . flymake-show-diagnostics-buffer)))

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :ensure t
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-eldoc-hook nil)
  (setq lsp-eldoc-enable-hover nil)
  :hook (lsp-mode . lsp-lens-mode)
  :bind(:map prog-mode-map
        ("C-c d d" . dap-debug)
        ("C-c d t". dap-breakpoint-toggle))
  :config
  (setq read-process-output-max (* 1024 1024 16)) ;; 1mb
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil))

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
  (latex-mode . company-mode)
  :custom
  (lsp-completion-provider :capf)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t))

(use-package posframe
  :ensure t
  :defer t)

(use-package dap-mode
  :ensure t
  :defer t
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
 ;; ================================================================================


;; ======================================== Groovy ========================================

(use-package groovy-mode
  :ensure t
  :defer t)

;; ================================================================================


;; ====================================== Python ==========================================
(use-package lsp-pyright
  :ensure t
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pyright")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyright-remote))
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
  :config
  (yas-reload-all))

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
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package intellij-features
  :load-path "custom-lisp"
  :defer t
  :hook ((c-mode c++-mode objc-mode java-mode) . (lambda ()
                    (require 'intellij-features)
                    (local-set-key (kbd "<backspace>") #'intellij-backspace))))

(use-package sis
  :ensure t
  :defer 1
  :config
  ;; For MacOS
  (sis-ism-lazyman-config "1" "2" 'fcitx5)
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t))

(use-package org
  :ensure nil
  :defer t
  :custom
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
  (org-latex-image-default-height "120pt")
  :config (message "Org"))

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
  (load-file "~/.emacs.d/custom-lisp/org-ext.el")
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
  :custom
  ;; use as-is if you don't have an existing org-agenda setup
  ;; otherwise push the directory to the existing list
  (org-agenda-files (list (expand-file-name "org-agenda" org-directory) org-gtd-directory))
  ;; (org-agenda-files `(,org-gtd-directory))
  ;; a useful view to see what can be accomplished today
  (org-agenda-custom-commands '(("g" "Scheduled today and all NEXT items" ((agenda "" ((org-agenda-span 1))) (todo "NEXT")))))
  :config (setq recentf-exclude (org-agenda-files)))


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
  (org-sketch-xournal-template-dir "~/.emacs.d/site-lisp/org-sketch/template")  ;; xournal 模板存储目录
  (org-sketch-xournal-default-template-name "template.xopp") ;; 默认笔记模版名称，应该位于 org-sketch-xournal-template-dir
  (org-sketch-apps '("xournal" "drawio")))

(use-package vterm
  :ensure t
  :defer t
  :bind (:map vterm-mode-map
              ("C-x C-q" . vterm-copy-mode)
         :map vterm-copy-mode-map
              ("C-x C-q" . vterm-copy-mode)))

(use-package quickrun
  :ensure t
  :defer t)

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

(when (daemonp)
  (menu-bar-mode +1)
  (global-tab-line-mode +1)
  (use-package right-click-context
    :ensure t
    :config (right-click-context-mode +1))
  (use-package bar-cursor
  :ensure t
  :config (bar-cursor-mode +1)))

(when (not window-system)
  (global-set-key (kbd "M-=") 'er/expand-region))

(provide 'init)
;;;
(put 'dired-find-alternate-file 'disabled nil)
