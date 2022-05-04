;;; -*- lexical-binding: t -*-

(require 'emms-player-simple)

(defconst emms-player-vgmstream-extensions '("2dx9" "aaap" "aax" "acm"
                                             "adp" "adpcm" "ads" "adx" "afc" "agsc" "ahx" "aifc" "aix" "amts" "as4"
                                             "asd" "asf" "asr" "ass" "ast" "at3" "aud" "aus" "baf" "baka" "bao"
                                             "bar" "bcstm" "bg00" "bgw" "bh2pcm" "bmdx" "bns" "bnsf" "bo2" "brstm"
                                             "caf" "capdsp" "ccc" "cfn" "cnk" "ddsp" "de2" "dec" "dmsg" "dsp" "dvi"
                                             "dxh" "eam" "emff" "enth" "fag" "filp" "fsb" "gca" "gcm" "gcsw" "gcw"
                                             "gms" "gsp" "hca" "hgc1" "his" "hps" "hwas" "idsp" "idvi" "ikm" "ild"
                                             "int" "isd" "ish" "ivaud" "ivb" "joe" "kces" "kcey" "khv" "kraw"
                                             "laac" "leg" "lflac" "logg" "lps" "lsf" "lstm" "lwav" "matx" "mc3"
                                             "mca" "mcg" "mi4" "mib" "mic" "mih" "mihb" "mpdsp" "msa" "msf" "mss"
                                             "msvp" "mta2" "mtaf" "mus" "musc" "musx" "mwv" "myspd" "ndp" "npsf"
                                             "nwa" "ogl" "p3d" "pcm" "pdt" "pk" "pnb" "ps2stm" "psh" "psw" "raw"
                                             "rkv" "rnd" "rrds" "rsd" "rsf" "rstm" "rwar" "rwav" "rws" "rwsd" "rwx"
                                             "rxw" "s14" "sab" "sad" "sap" "sb0" "sb1" "sb2" "sb3" "sb4" "sb5"
                                             "sb6" "sb7" "sc" "scd" "sd9" "sdt" "seg" "sfs" "sgb" "sgd" "sgh" "sgx"
                                             "sl3" "sm0" "sm1" "sm2" "sm3" "sm4" "sm5" "sm6" "sm7" "smp" "smpl"
                                             "snd" "sng" "sns" "sps" "spsd" "spw" "ss2" "ssm" "sss" "ster" "sth"
                                             "stm" "stma" "str" "strm" "sts" "stx" "svag" "svs" "swav" "swd" "tec"
                                             "thp" "tk5" "tydsp" "ulw" "um3" "vag" "vas" "vgs" "vig" "vjdsp" "voi"
                                             "vpk" "vs" "vsf" "waa" "wac" "wad" "wam" "was" "wavm" "wb" "wem" "wii"
                                             "wp2" "wsd" "wsi" "wvs" "xa" "xa2" "xa30" "xma" "xmu" "xss" "xvas"
                                             "xwav" "xwb" "xwh" "ydsp" "ymf" "zsd" "zwdsp"))

(define-emms-simple-player vgmstream '(file)
  (apply #'emms-player-simple-regexp emms-player-vgmstream-extensions)
  "vgmstream123")

(define-emms-simple-player playgsf '(file)
  (emms-player-simple-regexp "gsf" "minigsf")
  "playgsf" "-c" "-q")

(defconst emms-player-gme-extensions '("ay" "gbs" "gym" "hes" "kss" "nsf" "nsfe" "sap" "spc" "vgm" "vgz"))

(define-emms-simple-player gme '(file)
  (apply #'emms-player-simple-regexp  emms-player-gme-extensions)
  "gst-launch-1.0" "filesrc location='%s' ! gmedec ! autoaudiosink")

(defun emms-player-gme-start (track)
  "Start the player process."
  (emms-player-simple-start-with-params
   emms-player-gme emms-player-gme-command-name
   `("filesrc" "location" "=" ,(emms-track-name track) "!" "gmedec" "!" "autoaudiosink")))

(defun emms-player-simple-start-with-params (player cmdname params)
  (let ((process (apply #'start-process
                        emms-player-simple-process-name
                        nil
                        cmdname
                        params)))
    (set-process-sentinel process #'emms-player-simple-sentinel))
  (emms-player-started player))

(define-emms-simple-player audacious '(file)
  (apply #'emms-player-simple-regexp  (append emms-player-vgmstream-extensions emms-player-gme-extensions))
  "audacious" "-H")

(defvar emms-vgm-endless-loop-p nil)

(defun emms-vgm-toggle-endless-loop ()
  (interactive)
  (if emms-vgm-endless-loop-p
      (progn
        (remove "-c" emms-player-vgmstream-parameters)
        (remove "-e" 'emms-player-playgsf-parameters))
    (add-to-list 'emms-player-vgmstream-parameters "-c")
    (add-to-list 'emms-player-playgsf-parameters "-e"))
  (message "VGM endless loop %s" (if (setq emms-vgm-endless-loop-p (not emms-vgm-endless-loop-p)) "enabled" "disabled")))

(provide 'emms-vgm)
