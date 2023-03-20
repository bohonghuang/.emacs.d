;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(setf emms-player-mpv-ipc-method 'ipc-server)

(defvar emms-player-mpv-proc-init-with-celluloid-p nil)

(defmacro maybe-native-compile (name)
  (if (fboundp 'native-compile)
      `(native-compile ,name)
    `(byte-compile ,name)))

(maybe-native-compile
 (defun emms-player-mpv-proc-init-with-celluloid (fun &rest args)
   (let ((emms-player-mpv-command-name "celluloid")
         (emms-player-mpv-proc-init-with-celluloid-p t)
         (emms-player-mpv-parameters (cl-remove "--no-audio-display" emms-player-mpv-parameters :test #'string=)))
     (apply fun args))))

(advice-add #'emms-player-mpv-proc-init :around #'emms-player-mpv-proc-init-with-celluloid)

(maybe-native-compile
 (defun make-process-transform-celluloid-args (args)
   (when emms-player-mpv-proc-init-with-celluloid-p
     (setf (cl-getf args :command)
           (cl-loop with after-command-name-p = nil and before-file-p = t
                    for cmd in (cl-getf args :command)
                    when (string= cmd emms-player-mpv-command-name)
                    do (setf after-command-name-p t)
                    when (string= cmd "--")
                    do (setf before-file-p nil)
                    when (and before-file-p after-command-name-p (string-prefix-p "--" cmd))
                    do (setf cmd (s-concat "--mpv-" (s-chop-prefix "--" cmd)))
                    collect cmd)))
   args))

(advice-add #'make-process :filter-args #'make-process-transform-celluloid-args)

(provide 'emms-celluloid)
