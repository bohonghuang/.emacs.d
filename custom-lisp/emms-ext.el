;;; -*- lexical-binding: t -*-

(require 'cl-lib)

(require 'emms)
(require 'emms-info-native)
(require 'emms-volume)
(require 'pretty-hydra)

(pretty-hydra-define hydra-emms
  (:color red
   :title (if-let ((track (emms-playlist-current-selected-track)))
              (emms-track-description track)
            "No playing track")
   :hint nil)
  ("File"
   (("o l"    (call-interactively #'emms-play-playlist)                  "Play Playlist")
    ("o o"    (call-interactively #'emms-play-file)                      "Play File")
    ("o d"    (call-interactively #'emms-play-directory-tree)            "Play Directory")
    ("o u"    (call-interactively #'emms-play-url)                       "Play URL"))
   "Playlist"
   (("a l"    (call-interactively #'emms-add-playlist)                   "Add Playlist")
    ("a o"    (call-interactively #'emms-add-file)                       "Add File")
    ("a d"    (call-interactively #'emms-add-directory-tree)             "Add Directory")
    ("a u"    (call-interactively #'emms-add-url)                        "Add URL")
    ("a a"    (call-interactively #'consult-emms-library)                "Add Library")
    ("S"      (emms-shuffle)                                             "Shuffle"))
   "Select"
   (("l l"    (call-interactively #'consult-emms-playlists)              "Consult Playlists")
    ("l b"    (call-interactively #'consult-emms-metaplaylist)           "Consult Metaplayerlist")
    ("l c"    (call-interactively #'consult-emms-current-playlist)       "Consult Current Playlist"))
   "Playback"
   (("<SPC>"  (emms-pause)                                               "Pause/Resume")
    ("n"      (emms-next)                                                "Next")
    ("p"      (emms-previous)                                            "Previous")
    ("f"      (emms-seek-forward)                                        "Seek Forward")
    ("b"      (emms-seek-backward)                                       "Seek Backward")
    ("k"      (emms-stop)                                                "Stop"))
   "Score"
   (("s"      (call-interactively #'emms-score-set-playing)              "Set Score"))
   "Lyrics"
   (("L"      (call-interactively #'emms-lyrics-switch-display-position) "Switch Display"))
   "Volume"
   (("+"      (emms-volume-raise)                                        "Raise Volume")
    ("-"      (emms-volume-lower)                                        "Lower Volume"))
   "Other"
   (("m"      (emms)                                                     "EMMS Buffer"))))

(defun emms-lyrics-toggle-display-buffer-auto-create-or-kill-wrapper (fun &rest args)
  (let ((w (and emms-lyrics-buffer (get-buffer-window emms-lyrics-buffer t))))
    (if emms-lyrics-display-buffer
        (progn (apply fun args)
               (when w
                 (kill-buffer emms-lyrics-buffer)
                 (setq emms-lyrics-display-buffer nil)))
      (unless w
          (emms-lyrics-create-buffer)
          (setq emms-lyrics-display-buffer t))
      (apply fun args))))

(advice-add #'emms-lyrics-toggle-display-buffer :around #'emms-lyrics-toggle-display-buffer-auto-create-or-kill-wrapper)

(defconst emms-lyrics-display-positions (ring-convert-sequence-to-ring '((nil nil nil) (t nil nil) (nil t nil) (nil nil t))))

(defun emms-lyrics-switch-display-position (arg)
  (interactive "P")
  (pcase-let ((`(,on-minibuffer ,on-modeline ,buffer) (ring-next emms-lyrics-display-positions (list emms-lyrics-display-on-minibuffer
                                                                                                     emms-lyrics-display-on-modeline
                                                                                                     emms-lyrics-display-buffer))))
    (unless (eq emms-lyrics-display-on-minibuffer on-minibuffer)
      (emms-lyrics-toggle-display-on-minibuffer))
    (unless (eq emms-lyrics-display-on-modeline on-modeline)
      (emms-lyrics-toggle-display-on-modeline))
    (unless (eq emms-lyrics-display-buffer buffer)
      (emms-lyrics-toggle-display-buffer)
      (when (and arg (buffer-live-p emms-lyrics-buffer))
        (emms-lyrics-buffer-to-new-frame)))))

(cl-pushnew "lyrics" emms-info-native-vorbis--accepted-fields :test #'string=)

(defconst emms-info-lyrics-temp-file (make-temp-file "emms-info-lyrics-" nil ".lrc"))

(defun emms-info-native-vorbis--split-comment-multiline (comment)
  (let ((comment-string (decode-coding-string (mapconcat
                                               #'byte-to-string
                                               comment
                                               nil)
                                              'utf-8)))
    (when (string-match "^\\(.+?\\)=\\([\0-\377[:nonascii:]]*\\)" comment-string)
      (cons (downcase (match-string 1 comment-string))
            (match-string 2 comment-string)))))

(advice-add #'emms-info-native-vorbis--split-comment :override #'emms-info-native-vorbis--split-comment-multiline)

(defun emms-lyrics-find-with-info-lyric (file)
  (if-let ((file (emms-lyrics-find-lyric file)))
      file
    (when-let ((embedded-lyric (emms-track-get (emms-playlist-current-selected-track) 'info-lyrics)))
      (with-temp-buffer
        (insert embedded-lyric)
        (write-region (point-min) (point-max) emms-info-lyrics-temp-file nil))
      emms-info-lyrics-temp-file)))

(setq emms-lyrics-find-lyric-function #'emms-lyrics-find-with-info-lyric)

(defun emms-lyrics-delete-temp-file ()
  (when (file-exists-p emms-info-lyrics-temp-file)
    (delete-file emms-info-lyrics-temp-file)))

(add-hook 'kill-emacs-hook #'emms-lyrics-delete-temp-file)
(advice-add #'emms-lyrics-visit-lyric :after #'auto-revert-mode)

(defun emms-lyrics-buffer-recenter-after-display (lyrics line)
  (when emms-lyrics-display-buffer
    (dolist (w (get-buffer-window-list emms-lyrics-buffer nil t))
      (with-selected-window w
        (when line
          (goto-char (point-min))
          (forward-line (1- line))
          (set-window-point w (point))
          (recenter-top-bottom))))))

(defun emms-lyrics-buffer-to-new-frame ()
  (mapc #'delete-window (get-buffer-window-list emms-lyrics-buffer))
  (with-selected-frame (make-frame '((undecorated . t)
                                     (unsplittable . t)
                                     (minibuffer . nil)
                                     (width . 40)
                                     (height . 25)
                                     (left . (- 0))
                                     (top . (- 0))
                                     (alpha-background . 75)))
    (with-current-buffer (switch-to-buffer emms-lyrics-buffer)
      (hide-mode-line-mode +1)
      (set-window-dedicated-p (selected-window) t))))

(define-minor-mode emms-lyrics-buffer-auto-scroll-mode
  "Minor mode to make `emms-lyrics-display-buffer' scroll automatically with current line in lyrics."
  :global t
  (if emms-lyrics-buffer-auto-scroll-mode
      (advice-add #'emms-lyrics-display :after #'emms-lyrics-buffer-recenter-after-display)
    (advice-remove #'emms-lyrics-display #'emms-lyrics-buffer-recenter-after-display)))

(emms-lyrics-buffer-auto-scroll-mode +1)

(defun emms-playlist-insert-tracks-from-playlist-or-funcall (fun track)
  (let ((file (emms-track-name track))
        (type (emms-track-type track)))
    (if (eq type 'file)
        (pcase (file-name-extension file)
          ("pls" (emms-source-pls-playlist file))
          ((or "m3u" "m3u8") (emms-source-m3u-playlist file))
          ((or "l" "lisp" "el") (emms-source-native-playlist file))
          (_ (funcall fun track)))
      (funcall fun track))))

(advice-add #'emms-playlist-insert-track :around #'emms-playlist-insert-tracks-from-playlist-or-funcall)

(defcustom emms-volume-amixer-device nil
  "The device to change volume."
  :type 'symbol
  :group 'emms-volume)

(defun emms-volume-amixer-change-with-device-and-card (amount)
  "Change amixer master volume by AMOUNT."
  (message "Playback channels: %s"
           (with-temp-buffer
             (when (zerop
                    (apply #'call-process (append (list "amixer" nil (current-buffer) nil)
                                                  (when-let ((device emms-volume-amixer-device))
                                                    (list "-D" (symbol-name emms-volume-amixer-device)))
                                                  (when-let ((card emms-volume-amixer-card))
                                                    (list "-c" (number-to-string card)))
                                                  (list "sset" emms-volume-amixer-control
                                                        (format "%d%%%s" (abs amount)
                                                                (if (< amount 0) "-" "+"))))))
               (if (re-search-backward "\\[\\([0-9]+%\\)\\]" nil t)
                   (match-string 1))))))

(advice-add #'emms-volume-amixer-change :override #'emms-volume-amixer-change-with-device-and-card)

(defun emms-player-mpv-ignore-error-wrapper (fun &rest args)
  (ignore-errors (apply fun args)))

(advice-add #'emms-player-mpv-ipc-filter :around #'emms-player-mpv-ignore-error-wrapper)

(provide 'emms-ext)
