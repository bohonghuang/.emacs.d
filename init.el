(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file) (load-file custom-file))

(custom-set-variables
 '(avy-single-candidate-jump nil)
 '(column-number-mode t)
 '(company-minimum-prefix-length 1)
 '(compilation-scroll-output t)
 '(display-line-numbers t)
 '(display-line-numbers-type 'relative)
 '(global-display-line-numbers-mode t)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(lsp-metals-install-version "0.10.6-M1+29-22f5a4b1-SNAPSHOT")
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(org-attach-use-inheritance t)
 '(org-download-display-inline-images nil)
 '(org-download-method 'attach)
 '(org-export-with-tags nil)
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-latex-compiler "xelatex")
 '(org-latex-custom-lang-environments '((Chinese "")))
 '(org-latex-default-packages-alist
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
     ("" "ctex" nil nil)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(warning-suppress-log-types '((comp)))
 '(winner-mode t))

(defalias 'yes-or-no-p 'y-or-n-p)

;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  ;; (setq package-archives
  ;;     '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ;;       ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t))

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Enable defer and ensure by default for use-package
;; (setq use-package-always-defer t
;;       use-package-always-ensure t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package jetbrains-darcula-theme
  :ensure t)
(load-theme 'jetbrains-darcula t)

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(global-set-key (kbd "C-?") 'undo-redo)

(use-package crux
  :ensure t)


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
  :config
  (setq cnfonts-personal-fontnames '(("JetBrains Mono") nil nil))
  (setq cnfonts-use-face-font-rescale t))
;; 让 cnfonts 随着 Emacs 自动生效。
;; 让 spacemacs mode-line 中的 Unicode 图标正确显示。
;; (cnfonts-set-spacemacs-fallback-fonts)

(use-package ligature
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
                                      "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(add-hook 'prog-mode-hook (lambda () (visual-line-mode t)))

(global-set-key (kbd "C-x o") 'ace-window)

;; (use-package gcmh
;;   :ensure t
;;   :init
;;   (gcmh-mode 1)
;;   :config
;;   (setq garbage-collection-messages t))

;; (use-package sticky-windows
;;   :load-path "site-lisp/"
;;   :bind(("C-x 1" . 'sticky-window-delete-other-windows)
;;         ("C-x 0" . 'sticky-window-delete-window)
;;         ("C-x 9" . 'sticky-window-keep-window-visible)))

(defun dedicate-window ()
  (interactive)
  (set-window-dedicated-p (selected-window) t))

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
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

(use-package projectile
  :ensure t
  :hook (prog-mode . projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; ======================================== Scala ========================================

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :ensure t
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :ensure t
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


;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-eldoc-hook nil)
  (setq lsp-eldoc-enable-hover nil)
  :hook  (scala-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  :bind(:map prog-mode-map
        ("C-c d d" . dap-debug)
        ("C-c d t". dap-breakpoint-toggle))
  :config
  (setq read-process-output-max (* 1024 1024 16)) ;; 1mb
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
)

(use-package lsp-metals
  :ensure t
  :custom
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off -Xmx8192m"))
  :hook (scala-mode . lsp))

(use-package lsp-ui
  :ensure t)

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t))

(use-package posframe
  :ensure t)

(use-package dap-mode
  :ensure t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )

;; ================================================================================

;; =======================================Rust =========================================

(use-package rustic
  :ensure t
  ;; :bind(:map rustic-mode-map
              ;; ("C-c p u" . rustic-cargo-run)
              ;; ("C-c p c" . rustic-compile))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-on-save t)
  ;; (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
)

;; (defun rk/rustic-mode-hook ()
;;   ;; so that run C-c C-c C-r works without having to confirm, but don't try to
;;   ;; save rust buffers that are not file visiting. Once
;;   ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
;;   ;; no longer be necessary.
;;   (when buffer-file-name
;;    (setq-local buffer-save-without-query t)))

;; (use-package lsp-mode
;;   :ensure
;;   :commands lsp
;;   :custom
;;   ;; what to use when checking on-save. "check" is default, I prefer clippy
;;   (lsp-rust-analyzer-cargo-watch-command "clippy")
;;   (lsp-eldoc-render-all t)
;;   (lsp-idle-delay 0.6)
;;   (lsp-rust-analyzer-server-display-inlay-hints t)
;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; ================================================================================


;; ======================================== Groovy ========================================

(use-package groovy-mode
  :ensure t)

;; ================================================================================


;; ====================================== Python ==========================================
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; (use-package jupyter
;;   :ensure t)

(use-package ein
  :ensure t)

(require 'dap-python)
(dap-register-debug-template "Python Program"
  (list :type "python"
        :args "-i"
        :cwd nil
        :env '(("DEBUG" . "1"))
        :target-module (expand-file-name "~/工程/Python")
        :request "launch"
        :name "Python Program"))
;; ================================================================================

;; ======================================== C++ ========================================

;; Clangd

(use-package which-key
  :ensure t)
(which-key-mode t)

;; 
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'objc-mode 'lsp)

(setq lsp-clients-clangd-args
      '("--header-insertion=never"))

;; CCLS

;; (use-package ccls
;;   :ensure t
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))

(use-package cmake-mode
  :ensure t)

(use-package qml-mode
  :ensure t)
;; (use-package company-qml
;;   :config (add-to-list 'company-backends 'company-qml)
;; )


(put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
(put 'compilation-read-command 'safe-local-variable #'booleanp)


;; (add-hook 'prog-mode-hook (lambda ()
;;                            (local-set-key (kbd "C-c p r") (lambda () (interactive)
;;                                                             (projectile-run-project nil)
;;                                                             ))
;;                            (local-set-key (kbd "C-c p c") (lambda () (interactive)
;;                                                             (projectile-compile-project nil)
;;                                                             ))
;;                            ))

;; ================================================================================

;; ======================================== PlatformIO ========================================

(use-package platformio-mode
  :ensure t)

;; ================================================================================

(use-package scad-mode
  :ensure t)
(use-package scad-preview
  :ensure t
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
  :config (vhdl-capf-enable))

(defun vhdl-capf-flatten (l) (-flatten l))
;; ====================================================================================

(use-package magit
  :ensure t)

(use-package yasnippet
  :ensure t
  :defer t
  :hook
  (prog-mode . yas-minor-mode)
  (org-mode . yas-minor-mode))


(global-set-key (kbd "C-S-d") 'delete-region)

(global-set-key (kbd "C-;") 'avy-goto-char-timer)
;; (global-set-key (kbd "C-.") 'avy-goto-char-2-below)
;; (global-set-key (kbd "C-,") 'avy-goto-char-2-above)

(use-package expand-region
  :ensure t)

(global-set-key (kbd "C-=") 'er/expand-region)

(use-package multiple-cursors
  :ensure t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; (setq recentf-max-menu-items 10)


;; (use-package company-posframe
;;   :ensure t)
;; (company-posframe-mode 1)

;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode))

;;  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

;; (defun company-mode/backend-with-yas (backend)
;;   (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
;;   backend
;; (append (if (consp backend) backend (list backend))
;;         '(:with company-yasnippet))))

;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
;;       "Enable yasnippet but disable it inline."
;;       (if (eq command 'prefix)
;;           (when-let ((prefix (funcall fun 'prefix)))
;;             (unless (memq (char-before (- (point) (length prefix))) '(?: ?. ?> ?\())
;;               prefix))
;;         (funcall fun command arg)))
;; (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline)

;; (setq org-image-actual-width 512);/ (display-pixel-width) 5))

;; (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(use-package sis
  :ensure t
  ;; :hook
  ;; enable the /follow context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))

  :config
  ;; For MacOS
  (sis-ism-lazyman-config "1" "2" 'fcitx5)
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t))
  ;; enable the /inline english/ mode for all buffers
  ;; (sis-global-inline-mode t)

(setq org-directory "~/文档/org")

(if (file-exists-p "~/.emacs.d/local.el") (load-file "~/.emacs.d/local.el"))

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))

(setq org-attach-id-dir (expand-file-name "org-attach/data" org-directory))

;(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file (expand-file-name "org-capture/captures.org" org-directory))

;; (use-package pdf-tools
;;   :ensure t
;;   :init (pdf-loader-install))


(use-package org-gtd
  :ensure t
  :after org
  :demand t ;; without this, the package won't be loaded, so org-agenda won't be configured
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
  :custom
  ;; use as-is if you don't have an existing org-agenda setup
  ;; otherwise push the directory to the existing list
  (org-agenda-files (list (expand-file-name "org-agenda" org-directory) org-gtd-directory))
  ;; (org-agenda-files `(,org-gtd-directory))
  ;; a useful view to see what can be accomplished today
  (org-agenda-custom-commands '(("g" "Scheduled today and all NEXT items" ((agenda "" ((org-agenda-span 1))) (todo "NEXT"))))))

(setq recentf-exclude (org-agenda-files))

(use-package org-capture
  :ensure nil
  ;; note that org-gtd has to be loaded before this
  :after org-gtd
  :config
  ;; use as-is if you don't have an existing set of org-capture templates
  ;; otherwise add to existing setup
  ;; you can of course change the letters, too
  (setq org-capture-templates `(("i" "Inbox"
                                 entry (file ,(org-gtd--path org-gtd-inbox-file-basename))
                                 "* %?\n%U\n\n  %i"
                                 :kill-buffer t)
                                ("l" "Todo with link"
                                 entry (file ,(org-gtd--path org-gtd-inbox-file-basename))
                                 "* %?\n%U\n\n  %i\n  %a"
                                 :kill-buffer t))))

(use-package org-download
  :ensure t)
(add-hook 'dired-mode-hook 'org-download-enable)
(setq org-noter-default-notes-file-names '("notes.org")
      org-noter-notes-search-path '((expand-file-name "org-noter" org-directory)))


(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :ensure t
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
  :bind (:map org-mode-map
         ("C-c t a" . org-transclusion-add)
         ("C-c t t" . org-transclusion-mode)))

(use-package htmlize
  :ensure t)

(use-package ox-reveal
  :ensure t
  ;; :custom
  ;; (org-reveal-root (concat "file://" (expand-file-name "~/.config/yarn/global/node_modules/reveal.js")))
  )

(defun org-media-note-hydra/body-with-sis-set-english ()
  (interactive)
  (sis-set-english)
  (org-media-note-hydra/body))

(use-package pretty-hydra
  :ensure t)

(use-package mpv
  :ensure t)

(use-package org-media-note
  :load-path "site-lisp/org-media-note"
  :hook (org-mode . org-media-note-mode)
  :bind(:map org-mode-map
             ("C-c m" . org-media-note-hydra/body-with-sis-set-english))
  :config
  (setq org-media-note-screenshot-image-dir (expand-file-name "org-media-note" org-directory)
        org-media-note-display-inline-images nil))

;; (use-package org-krita
;;   :load-path "site-lisp/org-krita"
;;   :config
;;   (add-hook 'org-mode-hook 'org-krita-mode))

;; (setq org-media-note-save-screenshot-p t)

(use-package org-link-edit
  :load-path "site-lisp/org-link-edit")

(use-package org-sketch
  :load-path "site-lisp/org-sketch"
  :hook (org-mode . org-sketch-mode)
  :bind(:map org-mode-map
             ("C-c s s" . org-sketch-insert))
  :init
  (setq org-sketch-xournal-template-dir "~/.emacs.d/site-lisp/org-sketch/template"  ;; xournal 模板存储目录
        org-sketch-xournal-default-template-name "template.xopp" ;; 默认笔记模版名称，应该位于 org-sketch-xournal-template-dir
        org-sketch-apps '("xournal" "drawio")  ;; 设置使用的sketch应用
        ))


;; (use-package symon)
;; (setq symon-delay 1)
;; (symon-mode)

;; (use-package nyan-mode
;;   :ensure t
;;   :config
;;   (setq nyan-animate-nyancat t)
;;   (setq nyan-wavy-trail t)
;;   (setq nyan-bar-length 20)
;;   (nyan-mode 1))


;; (use-package webkit
;;   :load-path "site-lisp/emacs-webkit"
;;   :config
;;   (require 'webkit-ace)
;;   (require 'webkit-dark))
;;  :bind ("s-b" 'webkit)) ;; Bind to whatever global key binding you want if you want

;; (use-package svg-clock)

(use-package vterm
  :commands (vterm)
  :ensure t)

(use-package quickrun
  :ensure t)



;; (add-hook 'prog-mode-hook (lambda ()
;;                             (local-set-key (kbd "C-c C-c") 'quickrun)))

;; (use-package imbot
;;   :ensure t
;;   :hook (org-media-note-hydra/body . imbot-mode)
;;   :preface (setq imbot--im-config 'imbot--fcitx5))


(load-file "~/.emacs.d/custom-lisp/org-assist.el")
(load-file "~/.emacs.d/custom-lisp/advance-words-count.el")

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
;; Set the title
  ;; (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner
  (setq dashboard-startup-banner 3)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever image/text you would prefer

  ;; Content is not centered by default. To center, set
  ;; (setq dashboard-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  ;; (setq dashboard-show-shortcuts nil)
)

(use-package command-log-mode
  :ensure t)

(use-package elmacro
  :ensure t)

(use-package mermaid-mode
  :ensure t)

(use-package ob-mermaid
  :ensure t)

(use-package pyim
  :ensure t
  :config
  ;; 金手指设置，可以将光标处的编码，比如：拼音字符串，转换为中文。
  (global-set-key (kbd "M-j") 'pyim-convert-string-at-point)

  ;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
  (define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)
  (setq-default pyim-english-input-switch-functions
              '(pyim-probe-program-mode))

  ;; 我使用全拼
  (pyim-default-scheme 'quanpin)
  ;; (pyim-default-scheme 'wubi)
  ;; (pyim-default-scheme 'cangjie)

  ;; pyim 探针设置
  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  ;; (setq-default pyim-english-input-switch-functions
  ;;               '(pyim-probe-dynamic-english
  ;;                 pyim-probe-isearch-mode
  ;;                 pyim-probe-program-mode
  ;;                 pyim-probe-org-structure-template))

  ;; (setq-default pyim-punctuation-half-width-functions
  ;;               '(pyim-probe-punctuation-line-beginning
  ;;                 pyim-probe-punctuation-after-punctuation))

  ;; 开启代码搜索中文功能（比如拼音，五笔码等）
  (pyim-isearch-mode 1)

  ;; 设置选词框的绘制方式
  (if (posframe-workable-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))

  ;; 显示5个候选词。
  (setq pyim-page-length 5)
  (setq default-input-method "pyim"))

(use-package pyim-basedict
  :ensure t
  :config
  (pyim-basedict-enable))

(use-package dim
  :ensure t
  :config
  (dim-major-names
   '((emacs-lisp-mode    "elisp")))
  (dim-minor-names
   '((auto-fill-function " ↵")
     (whitespace-mode    " _"  whitespace)
     (paredit-mode       " ()" paredit)
     (eldoc-mode         ""    eldoc)
     (projectile-mode " Proj")
     (pyim-isearch-mode ""))))

(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-info-asynchronously t)
  (emms-mode-line-disable)
  (emms-playing-time-disable-display)
  (require 'emms-info-libtag) ;;; load functions that will talk to emms-print-metadata which in turn talks to libtag and gets metadata
  (setq emms-info-functions '(emms-info-libtag))) ;;; make sure libtag is the only thing delivering metadata

(when (daemonp)
  (menu-bar-mode +1)
  (global-tab-line-mode +1)
  ;; (tab-bar-mode +1)
  ;; (add-hook 'find-file-hook (lambda () (run-at-time 0.001 nil (lambda ()
                         ;; (switch-to-buffer-other-tab (previous-buffer))))))
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
