(require 'calc-bin)

(defun calc-set-or-reset-bit-at-point ()
  (interactive)
  (unless (= calc-number-radix 2) (error "Radix must be set to 2"))
  (let ((bitn (- (save-excursion (search-forward-regexp "[0-1]\\b") (point)) (point)))
        (stackn (string-to-number (save-excursion (search-backward-regexp "\\([0-9]\\)+:" (save-excursion (beginning-of-line) (point)))
                                                  (match-string 1)))))
    (setf (car (nth stackn calc-stack))
          (logxor (car (nth stackn calc-stack)) (lsh 1 (- bitn 1)))))
  (calc-refresh))

(define-key calc-mode-map (kbd "b s") #'calc-set-or-reset-bit-at-point)

(provide 'calc-bin-ext)
