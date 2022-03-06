(require 'emms)
(require 'pretty-hydra)

(pretty-hydra-define hydra-emms
    (:color red
     :title (if-let ((track (emms-playlist-current-selected-track)))
                (emms-track-description track)
              "No playing track")
     :hint nil)
    ("File"
     (("l"     (call-interactively #'emms-play-playlist)       "Play Playlist")
      ("o"     (call-interactively #'emms-play-file)           "Play File")
      ("O"     (call-interactively #'emms-play-directory-tree) "Play Directory"))
     "Playlist"
     (("S"     (emms-shuffle)                                  "Shuffle"))
     "Playback"
     (("<SPC>" (emms-pause)                                    "Pause/Resume")
      ("n"     (emms-next)                                     "Next")
      ("p"     (emms-previous)                                 "Previous")
      ("f"     (emms-seek-forward)                             "Seek Forward")
      ("b"     (emms-seek-backward)                            "Seek Backward")
      ("k"     (emms-stop)                                     "Stop"))
     "Score"
     (("s"     (call-interactively #'emms-score-set-playing)   "Set Score"))
     "Other"
     (("m"     (emms)                                          "Emms Buffer"))))

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

(provide 'emms-ext)
