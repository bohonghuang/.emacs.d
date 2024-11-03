;; -*- lexical-binding: t; -*-

(use-package material-pbm-icons
  :quelpa (material-pbm-icons :fetcher github :repo "bohonghuang/material-pbm-icons" :files ("*.el" "pbm"))
  :demand t)

(use-package tool-bar
  :ensure nil
  :defer t
  :init
  (defun kill-local-tool-bar-map ()
    (kill-local-variable 'tool-bar-map))
  :hook ((prog-mode text-mode special-mode compilation-mode) . kill-local-tool-bar-map)
  :custom
  (tool-bar-mode t)
  (tool-bar-position 'bottom)
  (tool-bar-button-margin 25)
  (modifier-bar-mode t)
  :config
  (defun android-support-kill-buffer (arg)
    (interactive "p")
    (cond
     ((>= arg 4) (kill-buffer-and-window))
     ((>= arg 1) (kill-buffer))))
  (defun android-support-save-buffer ()
    (interactive)
    (if (and (buffer-modified-p) (or (derived-mode-p 'prog-mode) (derived-mode-p 'text-mode)))
        (save-buffer)
      (save-some-buffers)))
  (defun android-support-toggle-touch-screen-keyboard ()
    (interactive)
    (message
     "Touch screen keyboard %s"
     (if (setf touch-screen-display-keyboard (not touch-screen-display-keyboard))
         "enabled" "disabled")))
  (defcustom android-support-global-tool-bar-custom-commands nil "Custom commands on the Android global tool bar.")
  (defmacro define-android-support-global-tool-bar-custom-commands (n)
    `(progn . ,(cl-with-gensyms (command)
                 (cl-loop for i from 1 to n
                          collect `(defun ,(intern (format "%s-%d" 'android-support-global-tool-bar-custom-command i)) ()
                                     (interactive)
                                     (when-let ((,command (nth ,(1- i) android-support-global-tool-bar-custom-commands)))
                                       (call-interactively ,command)))))))
  (define-android-support-global-tool-bar-custom-commands 6)
  (defconst android-support-global-tool-bar-items
    '(("close-outline" keyboard-quit)                 ("plus-circle-multiple-outline" universal-argument)  ("file-replace-outline" consult-buffer switch-to-buffer)  ("arrow-u-left-top" undo)    ("arrow-up" previous-line) ("content-save-outline" android-support-save-buffer save-buffer) ("numeric-1-circle-outline" android-support-global-tool-bar-custom-command-1) ("numeric-2-circle-outline" android-support-global-tool-bar-custom-command-2) ("numeric-3-circle-outline" android-support-global-tool-bar-custom-command-3) ("menu" imenu)
      ("arrow-collapse-right" indent-for-tab-command) ("circle-multiple-outline" execute-extended-command) ("close-circle-multiple-outline" exchange-point-and-mark) ("arrow-left" backward-char) ("arrow-down" next-line)   ("arrow-right" forward-char)                                     ("numeric-4-circle-outline" android-support-global-tool-bar-custom-command-4) ("numeric-5-circle-outline" android-support-global-tool-bar-custom-command-5) ("numeric-6-circle-outline" android-support-global-tool-bar-custom-command-6) ("magnify" isearch-forward)))
  (defun android-support-global-tool-bar-setup ()
    (setf tool-bar-map '(keymap nil))
    (cl-loop for (icon command key) in android-support-global-tool-bar-items
             do (tool-bar-add-item icon command (or key command))))
  (android-support-global-tool-bar-setup)
  (define-key key-translation-map (kbd "<tool-bar> <keyboard-quit>") (kbd "C-g"))
  (define-key key-translation-map (kbd "<tool-bar> <execute-extended-command>") (kbd "C-c"))
  (define-key key-translation-map (kbd "<tool-bar> <exchange-point-and-mark>") (kbd "C-x"))
  (define-key key-translation-map (kbd "<tool-bar> <imenu>") (kbd "M-g"))
  (define-key key-translation-map (kbd "<tool-bar> <isearch-forward>") (kbd "M-s"))
  (global-set-key (kbd "C-x <up>") #'delete-other-windows)
  (global-set-key (kbd "C-x <down>") #'split-window-below)
  (global-set-key (kbd "C-c <tool-bar> <universal-argument>") #'execute-extended-command)
  (global-set-key (kbd "C-x <tool-bar> <universal-argument>") #'er/expand-region)
  (global-set-key (kbd "C-c <tool-bar> <undo>") #'pop-to-mark-command)
  (global-set-key (kbd "C-x <tool-bar> <undo>") #'quit-window)
  (global-set-key (kbd "C-c <tool-bar> <switch-to-buffer>") #'project-find-file)
  (global-set-key (kbd "C-x <tool-bar> <switch-to-buffer>") #'find-file)
  (global-set-key (kbd "C-c <tool-bar> <save-buffer>") #'bookmark-set)
  (global-set-key (kbd "C-x <tool-bar> <save-buffer>") #'android-support-kill-buffer)
  (global-set-key (kbd "M-s M-s") #'isearch-forward)
  (global-set-key (kbd "M-g M-s") #'consult-imenu)
  (global-set-key (kbd "M-s M-g") (if (executable-find "rg") #'consult-ripgrep #'consult-grep))
  (global-set-key (kbd "C-x M-g") #'android-support-toggle-touch-screen-keyboard)
  (global-set-key (kbd "C-x M-s") #'read-only-mode)
  (define-key key-translation-map (kbd "<tool-bar> <indent-for-tab-command>") (kbd "TAB"))
  (define-key key-translation-map (kbd "<tool-bar> <previous-line>") (kbd "<up>"))
  (define-key key-translation-map (kbd "<tool-bar> <next-line>") (kbd "<down>"))
  (define-key key-translation-map (kbd "<tool-bar> <backward-char>") (kbd "<left>"))
  (define-key key-translation-map (kbd "<tool-bar> <forward-char>") (kbd "<right>")))

(provide 'android-support)
;;; android-support.el ends here
