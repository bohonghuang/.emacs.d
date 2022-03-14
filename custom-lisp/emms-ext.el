(require 'emms)
(require 'pretty-hydra)

(pretty-hydra-define hydra-emms
  (:color red
   :title (if-let ((track (emms-playlist-current-selected-track)))
              (emms-track-description track)
            "No playing track")
   :hint nil)
  ("File"
   (("o l"     (call-interactively #'emms-play-playlist)       "Play Playlist")
    ("o o"     (call-interactively #'emms-play-file)           "Play File")
    ("o d"     (call-interactively #'emms-play-directory-tree) "Play Directory"))
   "Playlist"
   (("S"     (emms-shuffle)                                    "Shuffle"))
   "Playback"
   (("<SPC>" (emms-pause)                                      "Pause/Resume")
    ("n"     (emms-next)                                       "Next")
    ("p"     (emms-previous)                                   "Previous")
    ("f"     (emms-seek-forward)                               "Seek Forward")
    ("b"     (emms-seek-backward)                              "Seek Backward")
    ("k"     (emms-stop)                                       "Stop"))
   "Score"
   (("s"     (call-interactively #'emms-score-set-playing)     "Set Score"))
   "Lyrics"
   (("l v"   (emms-lyrics-visit-lyric)                         "View Lyrics")
    ("l t"   (emms-lyrics-switch-display-position)             "Toggle Display"))
   "Other"
   (("m"     (emms)                                            "Emms Buffer"))))

(defconst emms-lyrics-display-positions (ring-convert-sequence-to-ring '((t . nil) (nil . t) (nil . nil))))

(defun emms-lyrics-switch-display-position ()
  (interactive)
  (pcase-let ((`(,on-minibuffer . ,on-modeline)
               (ring-next emms-lyrics-display-positions
                          (cons emms-lyrics-display-on-minibuffer emms-lyrics-display-on-modeline))))
    (unless (eq emms-lyrics-display-on-minibuffer on-minibuffer)
      (emms-lyrics-toggle-display-on-minibuffer))
    (unless (eq emms-lyrics-display-on-modeline on-modeline)
      (emms-lyrics-toggle-display-on-modeline))))

(push "lyrics" emms-info-native--accepted-vorbis-fields)

(defconst emms-info-lyrics-temp-file (make-temp-file "emms-info-lyrics-" nil ".lrc"))

(defun emms-info-native--split-vorbis-comment-multiline (comment)
  (let ((comment-string (decode-coding-string (mapconcat
                                               #'byte-to-string
                                               comment
                                               nil)
                                              'utf-8)))
    (when (string-match "^\\(.+?\\)=\\([\0-\377[:nonascii:]]*\\)" comment-string)
      (cons (downcase (match-string 1 comment-string))
            (match-string 2 comment-string)))))

(advice-add #'emms-info-native--split-vorbis-comment :override #'emms-info-native--split-vorbis-comment-multiline)

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

(provide 'emms-ext)
