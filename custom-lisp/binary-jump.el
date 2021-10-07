(require 'hl-line)

(defvar binary-jump-vertical-range nil)

(defvar binary-jump-round-up t)

(defun binary-jump-line-char (&optional arg)
  (let* ((screen-lines-from-top (count-screen-lines (save-excursion (move-to-window-line 0) (point)) (point)))
         (screen-lines-to-bottom (count-screen-lines (point) (save-excursion (move-to-window-line -1) (point))))
         (step-up screen-lines-from-top)
         (step-down (- screen-lines-to-bottom 1)))
    (catch 'break
      (unwind-protect
          (while t
            (progn
              (unless (overlayp hl-line-overlay)
                (setq hl-line-overlay (hl-line-make-overlay)))
              (overlay-put hl-line-overlay
                           'window (unless hl-line-sticky-flag (selected-window)))
              (hl-line-move hl-line-overlay)
              (hl-line-maybe-unhighlight))
            (pcase (or (car arg) (read-key))
              (`?p (setq step-down (/ (+ step-up (if binary-jump-round-up 1 0)) 2)
                         step-up (- step-up step-down))
                   (previous-line (max step-down 1)))
              (`?n (setq step-up (/ (+ step-down (if binary-jump-round-up 1 0)) 2)
                         step-down (- step-down step-up))
                   (next-line (max step-up 1)))
              (`7 (throw 'break nil))
              (_  (throw 'break t)))
            (when (not (null arg))
              (pop arg)))
        (hl-line-highlight)))))

(defun binary-jump-toward (dir)
  (pcase dir
    ((or `up `down)
     (let ((screen-lines-from-top (count-screen-lines (save-excursion (move-to-window-line 0) (point)) (point)))
           (screen-lines-to-bottom (count-screen-lines (point) (save-excursion (move-to-window-line -1) (point)))))
       (unless (and binary-jump-vertical-range (string-prefix-p "binary-jump" (format "%s" last-command)))
         (setq binary-jump-vertical-range (cons (- screen-lines-from-top 1) (- screen-lines-to-bottom 1))))
       (pcase dir
         (`up (setf (cdr binary-jump-vertical-range) (/ (+ (car binary-jump-vertical-range) 1) 2)
                    (car binary-jump-vertical-range) (- (car binary-jump-vertical-range) (cdr binary-jump-vertical-range)))
              (previous-line (max (cdr binary-jump-vertical-range) 1))
              (when (= screen-lines-from-top 0) (setq binary-jump-vertical-range nil)))
         (`down (setf (car binary-jump-vertical-range) (/ (+ (cdr binary-jump-vertical-range) 1) 2)
                      (cdr binary-jump-vertical-range) (- (cdr binary-jump-vertical-range) (car binary-jump-vertical-range)))
                (next-line (max (car binary-jump-vertical-range) 1))
                (when (= screen-lines-to-bottom 0) (setq binary-jump-vertical-range nil))))))))

(defmacro with-mark-set (&rest body)
  `(let ((mark (point)))
     (if (progn ,@body)
         (unless mark-active (push-mark mark))
       (progn (goto-char mark)))))

(defun binary-jump-previous-line ()
  (interactive)
  (binary-jump-toward 'up))

(defun binary-jump-next-line ()
  (interactive)
  (binary-jump-toward 'down))


(defun binary-jump-command ()
  (interactive)
  (with-mark-set (or (binary-jump-line) (call-interactively #'avy-goto-char-in-line))))

(defun visual-line-number-at-pos ()
  (interactive)
  (count-screen-lines
   (point-min)
   (save-excursion (beginning-of-visual-line) (point)))) ;这个函数可以计算从buffer开始到光标视觉上的行数

(provide 'binary-jump)
