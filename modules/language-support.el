;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-generic)

(defcustom language-support
  (unless (boundp 'language-support)
    (intern (completing-read "Select language support: " '("lsp-mode" "eglot" "citre" "nil") nil t)))
  "The language support used in `prog-mode'.")

(defcustom language-support-languages nil
  "All programming languages that this configured Emacs need to support.")

(defvar language-support-enabled-directories nil
  "The list of the directory where language support is enabled by user.")

(defun language-support-directory ()
  (or (when-let ((proj (project-current))) (project-root proj))
      (when-let ((file (buffer-file-name))) (file-name-directory file))))

(cl-defgeneric language-support--enable ()
  (:method (&context (major-mode prog-mode) (language-support (eql 'lsp-mode))) (lsp))
  (:method (&context (major-mode prog-mode) (language-support (eql 'eglot))) (eglot-ensure))
  (:method (&context (major-mode prog-mode) (language-support (eql 'citre))) (citre-mode)))

(defun language-support-disjoin (&rest predicates)
  (lambda (&rest args) (cl-loop for predicate in predicates thereis (apply predicate args))))

(defun language-support-file-mode-descendant-of-p (elem-a elem-b)
  (and (f-descendant-of-p (car elem-a) (car elem-b)) (or (eq (cdr elem-a) (cdr elem-b)) (let ((major-mode (cdr elem-a))) (derived-mode-p (cdr elem-b))))))

(defun language-support-enable (arg)
  (interactive "p")
  (let ((current-buffer (current-buffer))
        (directory-mode (cons (if (> arg 1) (read-directory-name "Enable language support in directory: " nil nil t nil)
                                (language-support-directory))
                              major-mode)))
    (if (cl-member (cons (buffer-file-name current-buffer) (buffer-local-value 'major-mode current-buffer))
                   language-support-enabled-directories :test #'language-support-file-mode-descendant-of-p)
        (language-support--enable)
      (cl-loop initially (language-support--enable)
               for buffer in (buffer-list)
               for file-name = (buffer-file-name buffer)
               for buffer-mode = (buffer-local-value 'major-mode buffer)
               when file-name
               when (language-support-file-mode-descendant-of-p (cons file-name buffer-mode) directory-mode)
               unless (eq current-buffer buffer)
               when (let ((hooksym (intern (concat (symbol-name major-mode) "-hook"))))
                      (and (boundp hooksym) (cl-member #'language-support-auto-enable (symbol-value hooksym))))
               do (with-current-buffer buffer (language-support--enable))
               finally (push directory-mode language-support-enabled-directories)))))

(cl-defgeneric language-support--disable ()
  (:method (&context (major-mode prog-mode) (language-support (eql 'lsp-mode))) (ignore-errors (lsp-workspace-shutdown (lsp--read-workspace))))
  (:method (&context (major-mode prog-mode) (language-support (eql 'eglot))) (ignore-errors (eglot-shutdown (eglot--current-server-or-lose))))
  (:method (&context (major-mode prog-mode) (language-support (eql 'citre)))))

(defun language-support-disable (arg)
  (interactive "p")
  (let ((directory-mode (cond
                         ((> arg 4)
                          (cons (read-directory-name "Disable language support in directory: " nil nil t) 'prog-mode))
                         ((> arg 1)
                          (let ((collection (cl-loop for elem in language-support-enabled-directories
                                                     collect (cons (format "%s" elem) elem))))
                            (alist-get (completing-read "Disable language support: " collection nil t) collection nil nil #'equal)))
                         (t (cons (language-support-directory) major-mode)))))
    (setf language-support-enabled-directories
          (cl-delete
           directory-mode language-support-enabled-directories
           :test (if (cl-member directory-mode language-support-enabled-directories :test #'equal)
                     #'equal (language-support-disjoin #'equal #'language-support-file-mode-descendant-of-p))))
    (cl-loop for buffer in (buffer-list)
             for file-name = (buffer-file-name buffer)
             for file-mode = (buffer-local-value 'major-mode buffer)
             when file-name
             when (language-support-file-mode-descendant-of-p (cons file-name file-mode) directory-mode)
             do (with-current-buffer buffer
                  (language-support--disable)
                  (flymake-mode -1)))))

(defun language-support-auto-enable ()
  (interactive)
  (when language-support
    (when-let ((file-name (buffer-file-name)))
      (when (cl-find (cons file-name major-mode) language-support-enabled-directories
                     :test #'language-support-file-mode-descendant-of-p)
        (call-interactively #'language-support-enable)))))

;;;;;;;;;;;;;;;
;; Languages ;;
;;;;;;;;;;;;;;;

(use-package elisp-mode
  :ensure nil
  :defer t
  :bind (:map emacs-lisp-mode-map
         ("C-c RET" . emacs-lisp-macroexpand)))

(use-package sly
  :when (member 'lisp language-support-languages)
  :ensure t
  :defer t
  :init
  (defmacro sly-define-lisp-implementations (impl-list)
    `(progn
       ,@(mapcar (lambda (impl)
                   (when (executable-find (symbol-name impl))
                     `(defun ,impl ()
                        ,(format "Start a %s session and connect to it." (upcase (symbol-name impl)))
                        (interactive)
                        (sly ,(symbol-name impl)))))
                 impl-list)))
  (sly-define-lisp-implementations (sbcl ccl abcl clasp clisp ecl))
  :bind (:map sly-inspector-mode-map
         ("<mouse-8>" . sly-inspector-pop))
  :custom
  (org-babel-lisp-eval-fn 'sly-eval))

(use-package sly-macrostep
  :when (member 'lisp language-support-languages)
  :ensure t
  :demand t
  :after sly
  :bind (:map sly-mode-map
         ("C-c C-<return>" . macrostep-expand))
  :config (cl-pushnew 'sly-macrostep sly-contribs))

(use-package geiser
  :when (member 'scheme language-support-languages)
  :ensure t
  :defer t)

(use-package macrostep-geiser
  :when (member 'scheme language-support-languages)
  :ensure t
  :demand t
  :after geiser-mode
  :bind (:map geiser-mode-map
         ("C-c C-<return>" . macrostep-geiser-expand-all))
  :hook (geiser-mode . macrostep-geiser-setup)
  :config (keymap-unset geiser-mode-map "M-`" t))

(use-package racket-mode
  :when (member 'racket language-support-languages)
  :ensure t
  :defer t
  :hook (racket-mode . racket-xp-mode))

(use-package ob-racket
  :when (member 'racket language-support-languages)
  :quelpa (ob-racket :fetcher github :repo "hasu/emacs-ob-racket")
  :defer t)

(use-package scala-mode
  :when (member 'scala language-support-languages)
  :ensure t
  :defer t
  :mode ("\\.sc\\'" . scala-mode)
  :hook (scala-mode . language-support-auto-enable))

(use-package ruby-mode
  :when (member 'ruby language-support-languages)
  :ensure t
  :defer t
  :hook (ruby-mode . language-support-auto-enable)
  :config (setf ruby-ts-mode-hook ruby-mode-hook))

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
   (setf sbt:program-options '("-Dsbt.supershell=false")))

(use-package cc-mode
  :when (cl-intersection '(c++ c objective-c java) language-support-languages)
  :ensure nil
  :defer t
  :init
  (defalias 'cpp-mode 'c++-mode)
  (defalias 'cpp-ts-mode 'c++-ts-mode)
  :hook ((c-mode c++-mode objc-mode java-mode) . language-support-auto-enable))

(use-package c-ts-mode
  :when (>= emacs-major-version 29)
  :ensure nil
  :defer t
  :config
  (require 'cc-mode)
  (setf c-ts-mode-hook c-mode-hook
        c++-ts-mode-hook c++-mode-hook
        objc-ts-mode-hook objc-mode-hook))

(use-package java-ts-mode
  :when (>= emacs-major-version 29)
  :ensure nil
  :defer t
  :config
  (setf java-ts-mode-hook java-mode-hook))

(use-package csharp-mode
  :when (and (member 'csharp language-support-languages) (>= emacs-major-version 29))
  :ensure nil
  :defer t
  :hook (csharp-mode . language-support-auto-enable)
  :config
  (setf csharp-ts-mode-hook csharp-mode-hook))

(use-package rustic
  :when (member 'rust language-support-languages)
  :ensure t
  :defer t
  :custom
  (rustic-lsp-setup-p nil)
  (rustic-lsp-client (when (member language-support '(lsp-mode eglot)) language-support))
  :hook ((rustic-mode rust-mode rust-ts-mode) . language-support-auto-enable)
  :config
  (defun language-support--enable-rustic (fun &rest args)
    (if (eq major-mode 'rustic-mode)
        (rustic-setup-lsp)
      (apply fun args)))
  (advice-add #'language-support--enable :around #'language-support--enable-rustic))

(use-package groovy-mode
  :when (member 'groovy language-support-languages)
  :ensure t
  :defer t
  :hook (groovy-mode . language-support-auto-enable)
  :config
  (setf groovy-ts-mode-hook groovy-mode-hook))

(use-package python
  :when (member 'python language-support-languages)
  :ensure nil
  :defer t
  :hook
  (inferior-python-mode . (lambda () (add-hook 'comint-output-filter-functions #'comint-truncate-buffer nil 'local)))
  (python-mode . language-support-auto-enable)
  :config
  (setf python-ts-mode-hook python-mode-hook)
  (defvar python-indent-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<") #'python-indent-shift-left)
      (define-key map (kbd ">") #'python-indent-shift-right)
      (dolist (it '(python-indent-shift-left python-indent-shift-right)) (put it 'repeat-map 'python-indent-repeat-map))
      map)
    "Keymap to repeat Python indentation key sequences.  Used in `repeat-mode'."))

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
  :hook (js-mode . language-support-auto-enable)
  :config
  (setf js-ts-mode-hook js-mode-hook))

(use-package typescript-mode
  :when (member 'typescript language-support-languages)
  :ensure t
  :defer t
  :mode ("\\.ts\\'" . typescript-mode)
  :preface (defalias 'ts-mode 'typescript-mode)
  :hook (typescript-mode . language-support-auto-enable)
  :config
  (setf typescript-ts-mode-hook typescript-mode-hook))

(use-package vhdl-capf
  :when (member 'vhdl language-support-languages)
  :ensure t
  :defer t
  :hook (vhdl-mode . vhdl-capf-enable)
  :config
  (defun vhdl-capf-flatten (l) (-flatten l)))

(use-package markdown-mode
  :when (member 'markdown language-support-languages)
  :ensure t
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
  :defer t
  :config
  (setf yaml-ts-mode-hook yaml-mode-hook))

(use-package toml-mode
  :when (member 'toml language-support-languages)
  :ensure t
  :defer t
  :hook (toml-mode . smartparens-mode)
  :config
  (setf toml-ts-mode-hook toml-mode-hook))

(use-package blueprint-mode
  :when (member 'blueprint language-support-languages)
  :quelpa (blueprint-mode :fetcher github :repo "DrBluefall/blueprint-mode")
  :defer t)

(use-package glsl-mode
  :when (member 'glsl language-support-languages)
  :ensure t
  :defer t
  :mode ("\\.\\(fs\\|vs\\)\\'" . glsl-mode)
  :hook (glsl-mode . language-support-auto-enable))

(use-package nim-mode
  :when (member 'nim language-support-languages)
  :ensure t
  :defer t
  :init
  (cl-defmethod language-support--enable (&context (major-mode (eql 'nim-mode)) (language-support t))
    (nimsuggest-mode +1))
  (cl-defmethod language-support--disable (&context (major-mode (eql 'nim-mode)) (language-support t))
    (nimsuggest-mode -1)
    (epc:stop-epc (cl-shiftf (alist-get (buffer-file-name (current-buffer)) nimsuggest--epc-processes-alist nil t #'equal) nil)))
  (defun nim-stop-buffer-nimsuggest ()
    (when-let ((epc (when (eq major-mode 'nim-mode) (cl-shiftf (alist-get (buffer-file-name (current-buffer)) nimsuggest--epc-processes-alist nil t #'equal) nil))))
      (epc:stop-epc epc)))
  :custom (nimsuggest-options nil)
  :hook
  (nim-mode . language-support-auto-enable)
  (kill-buffer . nim-stop-buffer-nimsuggest)
  :config
  (defvar nim-indent-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<") #'nim-indent-shift-left)
      (define-key map (kbd ">") #'nim-indent-shift-right)
      (dolist (it '(nim-indent-shift-left nim-indent-shift-right)) (put it 'repeat-map 'nim-indent-repeat-map))
      map)
    "Keymap to repeat Nim indentation key sequences.  Used in `repeat-mode'.")
  (when (eq system-type 'windows-nt)
    (define-advice nimsuggest--get-temp-file-name (:override () language-support)
      "Get temp file name."
      (mapconcat 'directory-file-name
                 `(,(nimsuggest--get-dirty-dir)
                   ,(cl-case system-type
                      ((ms-dos windows-nt cygwin)
                       ;; For bug #119, convert ":" to "꞉" (U+A789)
                       (concat "/"
                               (replace-regexp-in-string
                                ":" "s";;(char-to-string #xA789)
                                buffer-file-name)))
                      (t ; for *nix system
                       buffer-file-name)))
                 "")))
  (define-advice nim-capf--nimsuggest-complete (:override (prefix) language-support)
    ;; Note this function is not async function
    "Completion symbol of PREFIX at point using nimsuggest."
    (unless (or (nim-inside-pragma-p)
                (nim-syntax-comment-or-string-p))
      (nimsuggest--call-sync
       'sug (lambda (args) (nim-capf--format-candidates prefix args)))))
  (define-advice nimsuggest-flymake-setup (:before () language-support)
    (flymake-mode +1))
  (defun nim-toggle-corfu-company ()
    (interactive)
    (cl-assert (xor (bound-and-true-p company-mode) (bound-and-true-p corfu-mode)))
    (if (bound-and-true-p company-mode)
        (progn (company-mode -1) (call-interactively 'corfu-mode))
      (progn (corfu-mode -1) (call-interactively 'company-mode)))
    (nim-capf-setup))
  :commands (nim-toggle-corfu-company))

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
     :hook ((citre-mode . citre-tags-file-path))
     :custom
     (citre-project-root-function (lambda  () (project-root (or (project-current nil) (list 'vc nil default-directory)))))
     (citre-auto-enable-citre-mode-modes nil)
     (citre-gtags-args '("--compact"))))
  ('eglot
   (use-package eglot
     :defer t
     :ensure t
     :bind (:map eglot-mode-map
            ("C-c l r r" . eglot-rename)
            ("C-c l a a" . eglot-code-actions))
     :config
     (setq completion-category-defaults nil))

   (use-package eldoc-box
     :when (display-graphic-p)
     :defer t
     :ensure t
     :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
     :custom (eldoc-box-max-pixel-width 600)
     :config
     (defun eldoc-box--bottom-left-at-point-position-function (width height)
       (let* ((point-pos (eldoc-box--point-position-relative-to-native-frame))
              (x (car point-pos))
              (y (cdr point-pos))
              (em (frame-char-height)))
         (cons (if (< (- (frame-inner-width) width) x)
                   (max 0 (- x width))
                 x)
               (if (< y height)
                   (+ y em)
                 (- y height)))))
     (advice-add #'eldoc-box--default-at-point-position-function :override #'eldoc-box--bottom-left-at-point-position-function))

   (use-package eglot-tempel
     :ensure t
     :demand t
     :after eglot
     :config (eglot-tempel-mode +1))

   (use-package eglot-inactive-regions
     :ensure t
     :demand t
     :after eglot
     :custom
     (eglot-inactive-regions-style 'darken-foreground)
     (eglot-inactive-regions-opacity 0.4)
     :config
     (eglot-inactive-regions-mode +1))

   (use-package consult-eglot
     :ensure t
     :defer t
     :after eglot
     :bind (:map eglot-mode-map ("M-s s" . consult-eglot-symbols)))

   (use-package consult-eglot-embark
     :ensure t
     :demand t
     :after consult-eglot
     :config (consult-eglot-embark-mode +1)))
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
     (lsp-log-io nil)
     :config
     (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

   (use-package lsp-diagnostics
     :ensure lsp-mode
     :after lsp-mode
     :defer t
     :custom (lsp-diagnostic-package t))

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
     :bind(:map lsp-mode-map
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
            ("M-g I" . consult-lsp-symbols)
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
                       :major-modes '(scala-mode scala-ts-mode)
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
                       :major-modes '(c-mode c-ts-mode
                                      c++-mode c++-ts-mode
                                      objc-mode objc-ts-mode)
                       :remote? t
                       :server-id 'clangd-remote)))

   (use-package lsp-pyright
     :when (member 'python language-support-languages)
     :ensure t
     :defer t
     :config
     (lsp-register-client
      (make-lsp-client :new-connection (lsp-tramp-connection "pyright")
                       :major-modes '(python-mode python-ts-mode)
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
                       :major-modes '(java-mode java-ts-mode)
                       :remote? t
                       :server-id 'lsp-java-remote)))

   (use-package lsp-vhdl
     :when (member 'vhdl language-support-languages)
     :defer t
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

;;;;;;;;;;;;;
;; Treesit ;;
;;;;;;;;;;;;;

(use-package treesit-auto
  :when (require 'treesit nil t)
  :ensure t
  :defer nil
  :commands (global-treesit-auto-mode)
  :config (global-treesit-auto-mode +1))

(provide 'language-support)
;;; language-support.el ends here
