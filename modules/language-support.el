;; -*- lexical-binding: t; -*-

(defcustom language-support nil
  "The language support used in `prog-mode'.")

(defcustom language-support-languages nil
  "All programming languages that this configured Emacs need to support.")

(defvar language-support-enabled-directories nil
  "The list of the directory where language support is enabled by user.")

(defun language-support-directory ()
  (or (when-let ((proj (project-current))) (project-root proj))
      (when-let ((file (buffer-file-name))) (file-name-directory file))))

(defun language-support-enable ()
  "Enable language support for current project or directory."
  (interactive)
  (pcase language-support
    ('lsp-mode (lsp))
    ('eglot (eglot-ensure)))
  (add-to-list 'language-support-enabled-directories (language-support-directory) nil #'string-equal))

(defun language-support-auto-enable ()
  (when (member (language-support-directory) language-support-enabled-directories)
    (language-support-enable)))

;;;;;;;;;;;;;;;
;; Languages ;;
;;;;;;;;;;;;;;;

(use-package sly
  :when (member 'lisp language-support-languages)
  :ensure t
  :defer t
  :custom (org-babel-lisp-eval-fn 'sly-eval))

(use-package scala-mode
  :when (member 'scala language-support-languages)
  :ensure t
  :defer t
  :mode ("\\.sc\\'" . scala-mode)
  :interpreter
  ("scala" . scala-mode)
  :hook (scala-mode . language-support-auto-enable))

(use-package sbt-mode
  :when (member 'scala language-support-languages)
  :ensure t
  :defer t
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package cc-mode
  :when (cl-intersection '(c++ c objective-c) language-support-languages)
  :ensure nil
  :defer t
  :init (defalias 'cpp-mode 'c++-mode)
  :hook ((c-mode c++-mode objc-mode) . language-support-auto-enable))

(use-package rustic
  :when (member 'rust language-support-languages)
  :ensure t
  :defer t
  :custom
  (rustic-lsp-client nil)
  :hook (rustic-mode . language-support-auto-enable))

(use-package groovy-mode
  :when (member 'groovy language-support-languages)
  :ensure t
  :defer t
  :hook (groovy-mode . language-support-auto-enable))

(use-package python
  :when (member 'python language-support-languages)
  :ensure nil
  :defer t
  :hook
  (inferior-python-mode . (lambda () (add-hook 'comint-output-filter-functions #'comint-truncate-buffer nil 'local)))
  (python-mode . language-support-auto-enable)
  :config
  (defvar python-indent-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<") #'python-indent-shift-left)
      (define-key map (kbd ">") #'python-indent-shift-right)
      (dolist (it '(python-indent-shift-left python-indent-shift-right)) (put it 'repeat-map 'python-indent-repeat-map))
      map)
    "Keymap to repeat Python indentation key sequences.  Used in `repeat-mode'."))

(use-package ein
  :when (member 'python language-support-languages)
  :ensure t
  :defer t)

(use-package cmake-mode
  :when (member 'cmake language-support-languages)
  :ensure t
  :defer t)

(use-package json-mode
  :when (member 'json language-support-languages)
  :ensure t
  :defer t)

(use-package scad-mode
  :when (member 'scad language-support-languages)
  :ensure t
  :defer t
  :config
  (define-key scad-mode-map (kbd "<return>") nil))

(use-package scad-preview
  :when (member 'scad language-support-languages)
  :after scad-mode
  :ensure t
  :defer t
  :bind (:map scad-mode-map
         ("C-c C-c" . scad-preview-mode)))

(use-package js-mode
  :when (member 'javascript language-support-languages)
  :ensure nil
  :defer t
  :hook (js-mode . language-support-auto-enable))

(use-package typescript-mode
  :when (member 'typescript language-support-languages)
  :ensure t
  :defer t
  :mode ("\\.ts\\'" . typescript-mode)
  :hook (typescript-mode . language-support-auto-enable))

(use-package vhdl-capf
  :when (member 'vhdl language-support-languages)
  :ensure t
  :defer t
  :hook (vhdl-mode . vhdl-capf-enable)
  :config
  (defun vhdl-capf-flatten (l) (-flatten l)))

(use-package markdown-mode
  :when (member 'markdown language-support-languages)
  :ensure nil
  :defer t
  :init
  (defalias 'md-mode 'markdown-mode))

(use-package csv-mode
  :when (member 'csv language-support-languages)
  :ensure t
  :defer t
  :hook (csv-mode . csv-align-mode))

(use-package yaml-mode
  :when (member 'yaml language-support-languages)
  :ensure t
  :defer t)

(use-package toml-mode
  :when (member 'toml language-support-languages)
  :ensure t
  :defer t
  :hook (toml-mode . smartparens-mode))

;;;;;;;;;;;;;;;;;;;;;
;; Completion/Goto ;;
;;;;;;;;;;;;;;;;;;;;;

(pcase language-support
  ('citre
   (use-package citre
     :ensure t
     :defer t
     :init (require 'citre-config)
     :bind (("C-M-?" . citre-peek)
            :map citre-mode-map
            ("M-?" . citre-jump-to-reference))
     :custom
     (citre-auto-enable-citre-mode-modes '(prog-mode))
     (citre-project-root-function (lambda  () (project-root (project-current t))))
     (citre-gtags-args '("--compact")))
   (use-package citre-ext
     :load-path "custom-lisp"
     :demand t
     :hook ((after-save . citre-auto-update-tags-after-save)
            (citre-mode . citre-global-auto-objdir))
     :after citre))
  ('eglot
   (use-package eglot
     :defer t
     :ensure t
     :bind (:map prog-mode-map
                 ("C-c l r r" . eglot-rename)
                 ("C-c l a a" . eglot-code-actions))
     :config
     (setf (cdr (assoc 'python-mode eglot-server-programs)) '("pyright-langserver" "--stdio")
           (cdr (assoc 'scala-mode eglot-server-programs)) '("metals")
           (cdr (assoc 'rust-mode eglot-server-programs)) '("rust-analyzer")
           (cdr (assoc 'java-mode eglot-server-programs)) '("jdtls")
           (cdr (assoc '(tex-mode context-mode texinfo-mode bibtex-mode) eglot-server-programs)) '("texlab"))
     (setq completion-category-defaults nil)))
  ('lsp-mode
   (use-package lsp-mode
     :ensure t
     :defer t
     :init
     (setq lsp-keymap-prefix "C-c l")
     :custom
     (lsp-eldoc-hook nil)
     (lsp-eldoc-enable-hover nil)
     (lsp-completion-provider :none)
     (read-process-output-max (* 1024 1024 16))
     (lsp-idle-delay 0.5)
     (lsp-log-io nil))

   (use-package lsp-lens
     :ensure lsp-mode
     :after lsp-mode
     :defer t
     :hook (lsp-mode . lsp-lens-mode)
     :bind (:map lsp-mode-map
            ("C-c l l" . lsp-avy-lens)))
   
   (use-package dap-mode
     :after lsp-mode
     :ensure t
     :defer t
     :bind(:map dap-mode-map
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

   (use-package dap-ui
     :ensure lsp-mode
     :defer t
     :config
     (defun dap-ui--update-controls-change-border-color (frame)
       (when frame
         (set-face-background
          (if (facep 'child-frame-border)
              'child-frame-border
            'internal-border)
          (face-background 'default) frame)
         frame))
     (advice-add #'dap-ui--update-controls :filter-return #'dap-ui--update-controls-change-border-color))

   (use-package lsp-ui
     :ensure t
     :defer t
     :after lsp-mode
     :custom
     (lsp-ui-doc-show-with-cursor t)
     (lsp-ui-doc-position 'at-point)
     (lsp-ui-doc-delay 0.5)
     :config
     (defface lsp-ui-doc-border
       '((t :background "white"))
       "Border color of the documentation child frame.
     Only the `background' is used in this face."
       :group 'lsp-ui-doc)
     (setq lsp-ui-doc-border (face-background 'lsp-ui-doc-border nil t)))

   (use-package lsp-completion
     :ensure lsp-mode
     :defer t
     :init
     (defun lsp-completion-mode@after (&rest _) (kill-local-variable 'completion-category-defaults))
     (advice-add #'lsp-completion-mode :after #'lsp-completion-mode@after))

   (use-package consult-lsp
     :ensure t
     :defer t
     :after lsp-mode
     :bind (:map lsp-mode-map
                 ("M-g i" . consult-lsp-file-symbols)
                 ("M-g F" . consult-lsp-diagnostics)))

   (use-package yasnippet
     :ensure t
     :defer t
     :hook
     (lsp-mode . yas-minor-mode)
     :custom
     (yas-triggers-in-field t)
     (yas-indent-line 'fixed)
     :config
     (define-key yas-minor-mode-map [(tab)]        nil)
     (define-key yas-minor-mode-map (kbd "TAB")    nil)
     (define-key yas-minor-mode-map (kbd "<tab>")  nil))

   (use-package lsp-metals
     :when (member 'scala language-support-languages)
     :ensure t
     :defer t
     :custom
     (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off -Xmx8192m"))
     :config
     (lsp-register-client
      (make-lsp-client :new-connection (lsp-tramp-connection "metals")
                       :major-modes '(scala-mode)
                       :remote? t
                       :server-id 'metals-remote)))

   (use-package lsp-rust
     :when (member 'rust language-support-languages)
     :defer t
     :custom
     (lsp-rust-analyzer-proc-macro-enable t)
     :config
     (lsp-register-client
      (make-lsp-client :new-connection (lsp-tramp-connection "rust-analyzer")
                       :major-modes '(rustic-mode)
                       :remote? t
                       :server-id 'rust-analyzer-remote)))

   (use-package lsp-clangd
     :when (cl-intersection '(c++ c objective-c) language-support-languages)
     :defer t
     :custom
     (lsp-clients-clangd-args '("--header-insertion=never"))
     :config
     (lsp-register-client
      (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                       :major-modes '(c-mode c++-mode objc-mode)
                       :remote? t
                       :server-id 'clangd-remote)))

   (use-package lsp-pyright
     :when (member 'python language-support-languages)
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
     :when (member 'java language-support-languages)
     :ensure t
     :defer t
     :config
     (lsp-register-client
      (make-lsp-client :new-connection (lsp-tramp-connection "java-language-server")
                       :major-modes '(java-mode)
                       :remote? t
                       :server-id 'lsp-java-remote)))

   (use-package lsp-vhdl
     :when (member 'vhdl language-support-languages)
     :defer t
     :hook
     (vhdl-mode . (lambda () (ligature-mode -1)))
     :custom
     (lsp-vhdl-server 'ghdl-ls)
     (lsp-register-client
      (make-lsp-client :new-connection (lsp-tramp-connection "ghdl-ls")
                       :major-modes '(vhdl-mode)
                       :remote? t
                       :server-id 'ghdl-ls-remote)))
   (use-package lsp-tex
     :when (member 'tex language-support-languages)
     :defer t)))

(provide 'language-support)

