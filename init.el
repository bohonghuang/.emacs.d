;;; -*- lexical-binding: t -*-
(progn
  (let ((original-gc-cons-threshold gc-cons-threshold))
    (setq gc-cons-threshold most-positive-fixnum)
    (add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold original-gc-cons-threshold)))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file) (load-file custom-file))

(when (>= emacs-major-version 24)
  (require 'package)
  (dolist (archive '(("melpa" . "https://melpa.org/packages/")
                     ("gnu-devel" . "https://elpa.gnu.org/devel/")))
    (push archive package-archives)))

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

(use-package quelpa-use-package
  :demand t
  :ensure t
  :init (setq quelpa-update-melpa-p nil
              quelpa-use-package-inhibit-loading-quelpa t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Packages (Basic) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :defer nil
  :ensure nil
  :config (set-language-environment "UTF-8"))

(use-package emacs-ext
  :load-path "custom-lisp"
  :demand t)

(use-package minibuffer
  :ensure nil
  :defer nil
  :custom (history-length 1000))

(use-package package
  :ensure nil
  :defer nil
  :init (defalias 'ls-pkg 'list-packages))

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
  :custom (make-backup-files nil)
  :hook (find-file . (lambda ()
                       (unless (file-exists-p (file-truename buffer-file-name))
                         (set-buffer-file-coding-system 'utf-8)))))

(use-package url-handlers
  :ensure nil
  :defer nil
  :config (url-handler-mode +1))

(setq local-file (expand-file-name "local.el" user-emacs-directory))
(if (file-exists-p local-file) (load-file local-file))

(use-package window
  :ensure nil
  :defer nil
  :init (defalias 'window-buffer-change-hook 'window-buffer-change-functions)
  :config
  (defun previous-other-window ()
    (interactive)
    (other-window -1))
  (global-set-key (kbd "C-x O") #'previous-other-window)
  (define-key other-window-repeat-map (kbd "O") #'previous-other-window)
  (put #'previous-other-window 'repeat-map 'other-window-repeat-map)
  (defvar buffer-switch-repeat-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<left>") #'previous-buffer)
        (define-key map (kbd "<right>") #'next-buffer)
        (dolist (it '(previous-buffer next-buffer))
          (put it 'repeat-map 'buffer-switch-repeat-map))
        map)
      "Keymap to repeat window buffer navigation key sequences.  Used in `repeat-mode'."))

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

(use-package term/xterm
  :if (string-equal (getenv "TERM") "fbterm")
  :ensure nil
  :demand t
  :config
  (unless (terminal-coding-system)
    (set-terminal-coding-system 'utf-8-unix))
  (tty-no-underline)
  (ignore-errors (when gpm-mouse-mode (require 't-mouse) (gpm-mouse-enable)))
  (xterm-register-default-colors xterm-standard-colors))

(use-package monokai-theme
  :quelpa (monokai-theme :fetcher github :repo "BohongHuang/monokai-emacs")
  :if (null custom-enabled-themes)
  :config
  (load-theme 'monokai t))

;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Packages ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package hl-line
  :ensure nil
  :defer nil
  :hook (prog-mode . hl-line-mode))

(use-package hideshow
  :ensure nil
  :defer t
  :hook (prog-mode . hs-minor-mode))

(use-package paragraphs
  :ensure nil
  :defer nil
  :custom (sentence-end-double-space nil))

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
  (dired-listing-switches "-alh")
  (browse-url-handlers '(("\\`file:" . browse-url-default-browser)))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-async
  :ensure nil
  :demand t
  :after dired
  :config (dired-async-mode +1))

(use-package dired-ext
  :load-path "custom-lisp"
  :after dired
  :demand t)

(use-package diredfl
  :after dired
  :ensure t
  :defer t
  :hook (dired-mode . diredfl-mode))

(use-package dired-narrow
  :ensure t
  :defer t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package fd-dired
  :ensure t
  :defer t
  :init (defalias 'fd 'fd-dired))

(use-package savehist
  :ensure nil
  :defer nil
  :config (savehist-mode +1))

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

(use-package project
  :if (version<= "27.1" emacs-version)
  :ensure nil
  :defer t
  :config
  (put 'compile-command 'safe-local-variable #'stringp)
  (put 'compilation-read-command 'safe-local-variable #'booleanp))

(use-package project-ext
  :load-path "custom-lisp"
  :defer t
  :bind ("C-x p u" . project-run))

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

(use-package repeat
  :if (version<= "28.0.60" emacs-version)
  :ensure nil
  :defer nil
  :config
  (repeat-mode +1))

(use-package picture
  :ensure nil
  :defer t
  :bind (:map picture-mode-map ("C-c v" . picture-movement-down)))

;;;;;;;;;;;;;;;;;
;; Interaction ;;
;;;;;;;;;;;;;;;;;

(use-package ace-window
  :ensure t
  :defer t
  :bind (("M-o" . ace-window))
  :custom (aw-dispatch-always t))

(use-package pretty-hydra
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :defer 0.5
  :config (which-key-mode t))

(use-package doom-modeline
  :ensure t
  :defer nil
  :config
  (doom-modeline-mode +1))

(use-package vertico
  :ensure t
  :defer nil
  :config
  (vertico-mode +1))

(use-package vertico-directory
  :quelpa (vertico-directory :fetcher github :repo "minad/vertico" :files ("extensions/vertico-directory.el"))
  :defer t
  :bind(:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("C-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :defer t
  :hook (minibuffer-mode . marginalia-mode))

;; Example configuration for Consult
(use-package consult
  :ensure t
  :defer t
  :custom (completion-in-region-function #'consult-completion-in-region)
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-x M-x" . consult-mode-command)
         ("C-x C-k C-c" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g b" . consult-bookmark)
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
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
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
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package consult-dir
  :ensure t
  :demand t
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :ensure t
  :defer t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package crux
  :ensure t
  :defer t
  :bind (:map global-map
         ("C-x C-S-e" . crux-eval-and-replace)
         ("C-x C-S-f" . crux-open-with)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-<return>" . crux-smart-open-line)
         ("S-<f1>" . crux-find-user-init-file)))

;;;;;;;;;;
;; File ;;
;;;;;;;;;;

(use-package vlf
  :ensure t
  :defer t
  :init (require 'vlf-setup))

;;;;;;;;;;;
;; Fonts ;;
;;;;;;;;;;;

(use-package cnfonts
  :ensure t
  :defer t
  :bind (("C-M-_" . cnfonts-decrease-fontsize)
         ("C-M-+" . cnfonts-increase-fontsize)
         ("C-M-)" . cnfonts-reset-fontsize))
  :custom
  (cnfonts-personal-fontnames '(("JetBrains Mono" "JetBrainsMono Nerd Font") nil nil))
  (cnfonts-use-face-font-rescale t)
  (cnfonts-use-cache t)
  :config
  (advice-add #'cnfonts--step-fontsize :after (lambda (&rest _) (and (boundp 'doom-modeline-mode) doom-modeline-mode (doom-modeline-refresh-font-width-cache)))))

(use-package ligature
  :defer t
  :if (and (>= emacs-major-version 28) window-system)
  :hook (prog-mode . (lambda () (unless (-contains-p '(emacs-lisp-mode lisp-mode) major-mode) (require 'ligature) (ligature-mode +1))))
  :quelpa (ligature :fetcher github :repo "mickeynp/ligature.el")
  :config
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


;;;;;;;;
;; UI ;;
;;;;;;;;

(use-package posframe
  :ensure t
  :defer t)

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
                              "\\*cargo-run\\*"
                              "\\*Go-Translate\\*"
                              "\\*Compile-Log\\*"
                              "^\\*lsp-install"
                              help-mode
                              compilation-mode
                              dap-server-log-mode
                              xref--xref-buffer-mode
                              quickrun--mode
                              flymake-diagnostics-buffer-mode
                              flymake-project-diagnostics-mode))
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package smartparens
  :ensure t
  :defer t
  :hook
  ((prog-mode text-mode minibuffer-mode eshell-mode lisp-mode ielm-mode mermaid-mode) . smartparens-mode)
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
  (sp-highlight-wrap-tag-overlay nil)
  :config
  (use-package smartparens
    :after org
    :demand t
    :config
    (sp-local-pair 'org-mode "（" "）")
    (sp-local-pair 'org-mode "【" "】")
    (sp-local-pair 'org-mode "《" "》")
    (sp-local-pair 'org-mode "\\[" "\\]")
    (sp-local-pair 'org-mode "“" "”")))

(use-package indent-yank
  :quelpa (indent-yank :fetcher github :repo "BohongHuang/indent-yank")
  :defer t
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
   ("C-c '" . separedit))
  :config
  (add-to-list 'separedit-comment-faces 'tree-sitter-hl-face:comment))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook
  ((lisp-mode emacs-lisp-mode) . rainbow-delimiters-mode))

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

(if (version<= "29" emacs-version)
    (use-package pixel-scroll
      :ensure nil
      :defer nil
      :config
      (pixel-scroll-precision-mode +1))
  (use-package good-scroll
    :ensure t
    :defer 0.5
    :custom (good-scroll-step 100)
    :config
    (good-scroll-mode +1)))

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

(use-package flymake
  :ensure nil
  :defer t
  :hook (emacs-lisp-mode . flymake-mode)
  :bind
  ("C-c f f" . flymake-show-buffer-diagnostics)
  ("C-c f P" . flymake-show-project-diagnostics)
  ("C-c f n" . flymake-goto-next-error)
  ("C-c f p" . flymake-goto-prev-error)
  :config
  (defvar flymake-navigation-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'flymake-goto-next-error)
      (define-key map (kbd "p") #'flymake-goto-prev-error)
      (dolist (it '(flymake-goto-next-error flymake-goto-prev-error))
        (put it 'repeat-map 'flymake-navigation-repeat-map))
      map)
    "Keymap to repeat flymake navigation key sequences.  Used in `repeat-mode'."))

(use-package subword
  :ensure nil
  :defer t
  :hook ((c++-mode java-mode scala-mode rustic-mode) . subword-mode))

(use-package tree-sitter
  :ensure t
  :defer t
  :if (not (eq system-type 'windows-nt))
  :hook (prog-mode . (lambda ()
                       (require 'tree-sitter-langs)
                       (if (assoc major-mode tree-sitter-major-mode-language-alist)
                           (tree-sitter-mode +1))))
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :defer t
  :if (not (eq system-type 'windows-nt)))

(use-package company
  :ensure t
  :defer t
  :hook
  ((prog-mode ielm-mode) . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-frontends '(company-pseudo-tooltip-frontend
                          company-echo-metadata-frontend))
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t))

(use-package yasnippet
  :ensure t
  :defer t
  :hook
  ((prog-mode org-mode) . yas-minor-mode)
  :custom
  (yas-triggers-in-field t)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :hook (yas-minor-mode . (lambda () (require 'yasnippet-snippets)))) 

;;;;;;;;;;;;;;;;;;;;;;;
;; Language Supports ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package scala-mode
  :ensure t
  :defer t
  :mode ("\\.sc\\'" . scala-mode)
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :ensure t
  :defer t
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

(use-package rustic
  :ensure t
  :defer t
  :custom
  (rustic-lsp-client (if (boundp 'lsp-client) lsp-client 'eglot)))

(use-package groovy-mode
  :ensure t
  :defer t)

(use-package python
  :ensure nil
  :defer t
  :config
  (defvar python-indent-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<") #'python-indent-shift-left)
      (define-key map (kbd ">") #'python-indent-shift-right)
      (dolist (it '(python-indent-shift-left python-indent-shift-right)) (put it 'repeat-map 'python-indent-repeat-map))
      map)
    "Keymap to repeat Python indentation key sequences.  Used in `repeat-mode'."))

(use-package ein
  :ensure t
  :defer t)

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :defer t)

(use-package scad-mode
  :ensure t
  :defer t)

(use-package scad-preview
  :ensure t
  :defer t
  :bind(:map scad-mode-map
             ("C-c C-c" . scad-preview-mode)))

(use-package vhdl-capf
  :ensure t
  :defer t
  :hook (vhdl-mode . vhdl-capf-enable)
  :config
  (defun vhdl-capf-flatten (l) (-flatten l)))

(use-package markdown-mode
  :ensure nil
  :defer t
  :init
  (defalias 'md-mode 'markdown-mode))

(use-package csv-mode
  :ensure t
  :defer t
  :hook (csv-mode . csv-align-mode))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package toml-mode
  :ensure t
  :defer t)

;;;;;;;;;
;; LSP ;;
;;;;;;;;;

(pcase (and (boundp 'lsp-client) lsp-client)
  ((or `nil `eglot)
   (use-package eglot
     :hook ((scala-mode rustic-mode c++-mode c-mode objc-mode java-mode python-mode tex-mode) . eglot-ensure)
     :defer t
     :ensure t
     :bind (:map prog-mode-map
                 ("C-c l r" . eglot-rename)
                 ("C-c l a" . eglot-code-actions))
     :config
     (setf (cdr (assoc 'python-mode eglot-server-programs)) '("pyright-langserver" "--stdio")
           (cdr (assoc 'scala-mode eglot-server-programs)) '("metals")
           (cdr (assoc 'rust-mode eglot-server-programs)) '("rust-analyzer")
           (cdr (assoc 'java-mode eglot-server-programs)) '("jdtls")
           (cdr (assoc '(tex-mode context-mode texinfo-mode bibtex-mode) eglot-server-programs)) '("texlab"))))
  (`lsp-mode
   (use-package lsp-mode
     ;; Optional - enable lsp-mode automatically in scala files
     :ensure t
     :defer t
     :init
     (setq lsp-keymap-prefix "C-c l")
     :hook (lsp-mode . lsp-lens-mode)
     :custom
     (lsp-ui-doc-show-with-cursor t)
     (lsp-eldoc-hook nil)
     (lsp-eldoc-enable-hover nil)
     (lsp-completion-provider :capf)
     (read-process-output-max (* 1024 1024 16)) ;; 1mb
     (lsp-ui-doc-position 'at-point)
     (lsp-ui-doc-delay 0.5)
     (lsp-idle-delay 0.5)
     (lsp-log-io nil))

   (use-package lsp-lens
     :after lsp-mode
     :ensure nil
     :defer t
     :bind (:map lsp-mode-map
                 ("C-c l l" . lsp-avy-lens)))
   
   (use-package dap-mode
     :after lsp-mode
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

   (use-package lsp-ui
     :ensure t
     :defer t
     :after lsp-mode)

   (use-package consult-lsp
     :ensure t
     :defer t
     :after lsp-mode
     :bind (:map lsp-mode-map
                 ("M-g i" . consult-lsp-file-symbols)
                 ("M-g F" . consult-lsp-diagnostics)))

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

   (use-package lsp-rust
     :defer t
     :hook (rustic-mode . lsp)
     :custom
     (lsp-rust-analyzer-proc-macro-enable t)
     :config
     (lsp-register-client
      (make-lsp-client :new-connection (lsp-tramp-connection "rust-analyzer")
                       :major-modes '(rustic-mode)
                       :remote? t
                       :server-id 'rust-analyzer-remote)))

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

   (use-package lsp-vhdl
     :defer t
     :hook
     (vhdl-mode . lsp)
     (vhdl-mode . (lambda () (ligature-mode -1)))
     :custom
     (lsp-vhdl-server 'ghdl-ls)
     (lsp-register-client
      (make-lsp-client :new-connection (lsp-tramp-connection "ghdl-ls")
                       :major-modes '(vhdl-mode)
                       :remote? t
                       :server-id 'ghdl-ls-remote)))
   (use-package lsp-tex
     :defer t
     :hook
     (tex-mode . lsp))))

(use-package magit
  :defer t
  :ensure t)

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
    rust-mode
    rustic-mode
    js-mode
    tex-mode)
   .
   (lambda ()
     (require 'intellij-features)
     (local-set-key (kbd "DEL") #'intellij-backspace)
     (when (-contains-p '(java-mode rust-mode rustic-mode js-mode) major-mode)
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
  (calendar-mark-holidays-flag t)
  (calendar-chinese-all-holidays-flag t))

(use-package cal-china-x
  :ensure t
  :demand t
  :after calendar
  :config
  (setq mark-holidays-in-calendar t)
  (setq calendar-holidays cal-china-x-chinese-holidays))

;;;;;;;;;
;; Org ;;
;;;;;;;;;

(use-package org
  :ensure nil
  :defer t
  :custom
  (org-adapt-indentation nil)
  (org-export-with-tags nil)
  (org-default-notes-file (expand-file-name "org-capture/captures.org" org-directory))
  (org-latex-compiler "xelatex")
  (org-latex-custom-lang-environments '((Chinese "")))
  (org-hide-emphasis-markers t)
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
  :bind (:map org-mode-map
              ("M-," . org-mark-ring-goto)
              ("M-." . org-open-at-point)))

(use-package org-attach
  :after org
  :demand t
  :custom
  (org-attach-use-inheritance t)
  (org-attach-id-dir (expand-file-name "org-attach/data" org-directory)))

(use-package org-attach-refactor
  :quelpa (org-attach-refactor :fetcher github :repo "BohongHuang/org-attach-refactor")
  :defer t
  :commands org-attach-refactor-remove-id org-attach-refactor-add-id)

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
  :load-path "custom-lisp"
  :after org
  :demand t
  :bind (:map org-mode-map
         ("C-c C-S-L" . org-link-make-from-region)))

(use-package org-pomodoro
  :ensure t
  :defer t
  :bind (("C-c g f" . org-pomodoro))
  :custom (org-pomodoro-keep-killed-pomodoro-time t))

(use-package org-agenda
  :ensure nil
  :defer t
  :bind (:map org-mode-map
              ("C-c C-x C-S-o" . org-resolve-clocks)
         :map org-agenda-mode-map
              ("C-c C-x C-S-o" . org-resolve-clocks))
  :custom
  (org-agenda-files (list (expand-file-name "org-agenda" org-directory)))
  (org-agenda-window-setup 'current-window)
  :config
  (require 'recentf)
  (require 'org-gtd)
  (nconc recentf-exclude (org-agenda-files)))

(use-package org-gtd
  :ensure t
  :defer t
  :custom
  (org-gtd-directory (expand-file-name "org-gtd" org-directory))
  (org-edna-use-inheritance t)
  :bind
  (("C-c g c" . org-gtd-capture)
   ("C-c g e" . org-gtd-engage)
   ("C-c g p" . org-gtd-process-inbox)
   ("C-c g n" . org-gtd-show-all-next)
   ("C-c g s" . org-gtd-show-stuck-projects)
   ("C-c g a" . org-agenda-list)
   :map org-gtd-process-map
   ("C-c C-c" . org-gtd-choose))
  :config
  (org-edna-mode +1)
  (add-to-list 'org-agenda-files org-gtd-directory))

(use-package org-protocol
  :ensure nil
  :demand t
  :after org-capture)

(use-package org-download
  :defer t
  :ensure t
  :hook (org-mode . (lambda () (require 'org-download)))
  :custom
  (org-download-display-inline-images nil)
  (org-download-method 'attach))

(use-package org-appear
  :ensure t
  :defer t
  :hook (org-mode . org-appear-mode))

(use-package org-remark
  :when (and (boundp 'use-org-remark) use-org-remark)
  :ensure t
  :defer t
  :init (require 'org-remark-global-tracking)
  :custom
  (org-remark-global-tracking-mode +1)
  (org-remark-notes-file-path (expand-file-name "org-remark/notes.org" org-directory))
  (org-remark-notes-display-buffer-action '((display-buffer-in-side-window)
                                            (side . right)
                                            (slot . 1)))
  :bind (("C-c n m" . org-remark-mark)
        :map org-remark-mode-map
        ("C-c n o" . org-remark-open)
        ("C-c n n" . org-remark-view-next)
        ("C-c n p" . org-remark-view-prev)
        ("C-c n r" . org-remark-remove)
        ("C-c n c" . org-remark-change)
        ("C-c n ." . org-remark-view))
  :config
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
  :ensure t
  :defer t
  :custom
  (org-noter-default-notes-file-names '("notes.org"))
  (org-noter-notes-search-path (list (expand-file-name "org-noter" org-directory)))
  (org-noter-always-create-frame nil))

(when (and (boundp 'use-pdf-tools) use-pdf-tools)
  (use-package pdf-tools
    :ensure t
    :defer t
    :mode ("\\.pdf\\'" . (lambda ()
                           (require 'pdf-tools)
                           (pdf-tools-install)
                           (pdf-view-mode))))

  (use-package org-pdftools
    :ensure t
    :defer t
    :hook (org-mode . org-pdftools-setup-link))

  (use-package org-noter-pdftools
    :after pdf-tools
    :init
    (use-package org-noter-pdftools
        :after (pdf-tools org-noter)
        :demand t)
    :ensure t
    :defer t
    :bind (:map pdf-view-mode-map
                          ("i" . org-noter))
    :config
    ;; Add a function to ensure precise note is inserted
    (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
      (interactive "P")
      (org-noter--with-valid-session
       (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                     (not org-noter-insert-note-no-questions)
                                                   org-noter-insert-note-no-questions))
             (org-pdftools-use-isearch-link t)
             (org-pdftools-use-freestyle-annot t))
         (org-noter-insert-note (org-noter--get-precise-info)))))

    ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
    (defun org-noter-set-start-location (&optional arg)
      "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
      (interactive "P")
      (org-noter--with-valid-session
       (let ((inhibit-read-only t)
             (ast (org-noter--parse-root))
             (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
         (with-current-buffer (org-noter--session-notes-buffer session)
           (org-with-wide-buffer
            (goto-char (org-element-property :begin ast))
            (if arg
                (org-entry-delete nil org-noter-property-note-location)
              (org-entry-put nil org-noter-property-note-location
                             (org-noter--pretty-print-location location))))))))
    (with-eval-after-load 'pdf-annot
      (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note))))

(use-package org-roam
  :ensure t
  :defer t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (expand-file-name "org-roam" org-directory))
  (org-roam-graph-link-den-types '("file" "attachment"))
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ;; Dailies
         ("C-c r j" . org-roam-dailies-capture-today)
         ("C-c r t" . org-roam-buffer-toggle))
  :config
  (org-roam-setup)
  (require 'org-roam-protocol))

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

(use-package org-bars
  :if window-system
  :quelpa (org-bars :fetcher github :repo "tonyaldon/org-bars")
  :defer t
  :hook (org-mode . org-bars-mode))

(use-package org-indent
  :ensure nil
  :defer t
  :hook (org-mode . org-indent-mode))

(use-package ox-md
  :ensure nil
  :defer t
  :hook (org-mode . (lambda () (require 'ox-md))))

(use-package ox-reveal
  :ensure t
  :defer t
  :hook (org-mode . (lambda () (require 'ox-reveal))))

(use-package org-media-note
  :quelpa (org-media-note :fetcher github :repo "yuchen-lea/org-media-note")
  :defer t
  :hook (org-mode . org-media-note-mode)
  :bind(:map org-mode-map
             ("C-c m" . org-media-note-hydra/body-with-sis-set-english))
  :custom
  (org-media-note-display-inline-images nil)
  (org-media-note-screenshot-image-dir (expand-file-name "org-media-note" org-directory)))

(use-package org-tree-slide
  :ensure t
  :after org
  :defer t
  :bind (:map org-mode-map
         ("C-c s s" . org-tree-slide-mode)
         ("C-c s S-s" . 'org-tree-slide-skip-done-toggle)
         :map org-tree-slide-mode-map
              ("C-c s p" . org-tree-slide-move-previous-tree)
              ("C-c s n" . org-tree-slide-move-next-tree)))

(use-package org-englearn
  :quelpa (org-englearn :fetcher github :repo "BohongHuang/org-englearn")
  :defer t
  :commands org-englearn-capture org-englearn-process-inbox org-englearn-capture-process-region
  :bind
  (("C-c e c" . org-englearn-capture)
   ("C-c e p" . org-englearn-process-inbox)
   :map org-capture-mode-map
   ("C-c e c" . org-englearn-capture-process-region))
  :config
  (add-to-list 'org-capture-templates `("e" "English sentence"
   entry (file ,(expand-file-name "org-capture/english.org" org-directory))
   "* %?\n%U\n\n  %i\n  %a"
   :kill-buffer t)))

(use-package org-englearn-pdf-view
  :ensure nil
  :defer t
  :after pdf-view
  :custom (org-englearn-pdf-view-disable-org-pdftools-link t)
  :bind
  (:map pdf-view-mode-map
        ("C-c e c" . org-englearn-capture-pdf-view)))

;;;;;;;;;;;;
;; AUCTeX ;;
;;;;;;;;;;;;

(use-package tex
  :ensure auctex
  :defer t
  :hook
  (TeX-mode . company-mode)
  (TeX-mode . yas-minor-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (TeX-electric-math nil);'("$" . "$"))
  :config
  (when (boundp 'tex-mode-hook)
    (dolist (hook tex-mode-hook) (add-to-list 'TeX-mode-hook hook)))
  (when (boundp 'latex-mode-hook)
    (dolist (hook latex-mode-hook) (add-to-list 'LaTeX-mode-hook hook))))

(use-package go-translate
  :ensure t
  :defer t
  :custom
  (gts-translate-list '(("en" "zh")))
  :bind ("C-c t t" . gts-do-translate)
  :config
  (defalias 'subseq 'cl-subseq))

(use-package em-term
  :ensure nil
  :defer t
  :config
  (dolist (it '("nvtop" "bashtop" "btop" "top" "vim" "nvim" "cmatrix"))
    (add-to-list 'eshell-visual-commands it)))

(use-package eshell
  :ensure nil
  :defer t)

(use-package vterm
  :ensure t
  :defer t
  :bind (:map vterm-mode-map
              ("C-x C-q" . vterm-copy-mode)
         :map vterm-copy-mode-map
         ("C-x C-q" . vterm-copy-mode)))

(use-package eshell-vterm
  :ensure t
  :demand t
  :after eshell
  :config
  (eshell-vterm-mode +1))

(use-package quickrun
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
  (dolist (it '("C-v" "M-v" "S-<delete>" "<tab>")) (add-to-list 'rime-translate-keybindings it)))

(use-package secret-mode
  :quelpa (secret-mode :fetcher github :repo "bkaestner/secret-mode.el")
  :defer t
  :commands secret-mode
  :bind (("<pause>" . secret-mode)))

(use-package mpv
  :ensure t
  :defer t
  :custom (mpv-default-options '("--volume-max=300")))

(use-package emms
  :ensure t
  :defer t
  :commands emms
  :hook (emms-player-started . emms-show)
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-playlist-buffer-name "*Music*")
  (emms-info-asynchronously t)
  (emms-info-functions '(emms-info-libtag))
  :bind (("C-c m m" . emms)
         ("C-c m n" . emms-next)
         ("C-c m p" . emms-previous)
         :map emms-mark-mode-map
              ("n" . next-line)
              ("p" . previous-line))
  :config
  (require 'emms-setup)
  (require 'emms-info-libtag)
  (emms-all)
  (emms-default-players)
  (emms-mode-line-disable)
  (emms-playing-time-disable-display))

(use-package rsync-mode
  :quelpa (rsync-mode :fetcher github :repo "BohongHuang/rsync-mode")
  :ensure t
  :defer t)

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

(when (not window-system)
  (global-set-key (kbd "M-=") #'er/expand-region)
  (global-set-key (kbd "C-x ;") #'comment-line))

(provide 'init)
;;; init.el
