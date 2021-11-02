;;; -*- lexical-binding: t -*-
(progn
  (setq gc-cons-threshold most-positive-fixnum)
  (let ((original-gc-cons-threshold gc-cons-threshold))
    (add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold original-gc-cons-threshold)))))

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

(use-package quelpa-use-package
  :demand t
  :ensure t
  :init (setq quelpa-update-melpa-p nil
              quelpa-use-package-inhibit-loading-quelpa t))

(use-package package
  :ensure nil
  :defer nil
  :init (defalias 'ls-pkg 'list-packages))

(use-package emacs
  :defer nil
  :ensure nil
  :config (set-language-environment "UTF-8"))

(use-package emacs-ext
  :load-path "custom-lisp"
  :demand t)

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

(setq local-file (expand-file-name "local.el" user-emacs-directory))
(if (file-exists-p local-file) (load-file local-file))

(use-package monokai-theme
  :quelpa (monokai-theme :fetcher github :repo "HuangBoHong/monokai-emacs")
  :if (null custom-enabled-themes)
  :config
  (load-theme 'monokai t))

(use-package hl-line
  :ensure nil
  :defer nil
  :hook (prog-mode . hl-line-mode))

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
  (browse-url-handlers '(("\\`file:" . browse-url-default-browser)))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired+
  :quelpa (dired+ :fetcher wiki :url "https://www.emacswiki.org/emacs/dired+.el")
  :after dired
  :demand t
  :custom (diredp-hide-details-initially-flag nil))

(use-package fd-dired
  :ensure t
  :defer t
  :init (defalias 'fd 'fd-dired))

(use-package savehist
  :ensure nil
  :defer nil
  :config
  (savehist-mode +1))

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
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
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

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(use-package embark
  :ensure t
  :defer t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
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
  :quelpa (ligature :fetcher github :repo "mickeynp/ligature.el")
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
                              "\\*Go-Translate\\*"
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
  (sp-highlight-wrap-tag-overlay nil))

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

(use-package repeat
  :if (version<= "28.0.60" emacs-version)
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
  :ensure nil
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
  ((prog-mode ielm-mode) . company-mode)
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
    rust-mode
    rustic-mode
    js-mode)
   .
   (lambda ()
     (require 'intellij-features)
     (local-set-key (kbd "DEL") #'intellij-backspace)
     (local-set-key (kbd "{") #'intellij-left-bracket)
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
  (calendar-chinese-all-holidays-flag t))

(use-package org
  :ensure nil
  :defer t
  :custom
  (org-adapt-indentation nil)
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
                                    :kill-buffer t)
                                   ("e" "English sentence"
                                    entry (file ,(expand-file-name "org-capture/english.org" org-directory))
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
         ("C-c r j" . org-roam-dailies-capture-today)
         ("C-c r t" . org-roam-buffer-toggle))
  :config
  (org-roam-setup)
  (require 'org-roam-protocol))

(use-package org-transclusion
  :quelpa (org-transclusion :fetcher github :repo "nobiot/org-transclusion")
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

(use-package org-bars
  :if window-system
  :quelpa (org-bars :fetcher github :repo "tonyaldon/org-bars")
  :defer t
  :hook (org-mode . org-bars-mode))

(use-package org-indent
  :ensure nil
  :defer t
  :hook (org-mode . org-indent-mode))

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
  :defer t
  :bind (:map org-mode-map
         ("<f8>" . org-tree-slide-mode)
         ("S-<f8>" . 'org-tree-slide-skip-done-toggle)
         :map org-tree-slide-mode-map
              ("<f9>" . org-tree-slide-move-previous-tree)
              ("<f10>" . org-tree-slide-move-next-tree)))

(use-package org-englearn
  :quelpa (org-englearn :fetcher github :repo "HuangBoHong/org-englearn")
  :defer t
  :commands org-englearn-capture org-englearn-process-inbox org-englearn-capture-process-region
  :bind
  (("C-c e c" . org-englearn-capture)
   ("C-c e p" . org-englearn-process-inbox)
   :map org-capture-mode-map
   ("C-c e r" . org-englearn-capture-process-region)))

(use-package go-translate
  :ensure t
  :defer t
  :custom
  (gts-translate-list '(("en" "zh")))
  :bind ("C-c t t" . gts-do-translate)
  :config
  (defalias 'subseq 'cl-subseq))

(use-package calc-textrail
  :quelpa (calc-textrail :fetcher github :repo "HuangBoHong/calc-textrail")
  :commands calc-textrail-mode calc-textrail-preview
  :after calc
  :defer t
  :bind (:map calc-mode-map
              ("<f5>" . calc-textrail-preview)
              ("S-<f5>" . calc-textrail-mode)))


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
  :quelpa (secret-mode :fetcher github :repo "bkaestner/secret-mode.el")
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

(use-package rsync-mode
  :quelpa (rsync-mode :fetcher github :repo "HuangBoHong/rsync-mode")
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
                      #'(lambda() (edit-server-start)))))
  :custom (edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (top . 200)
                  (left . 200)
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . t))))

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
