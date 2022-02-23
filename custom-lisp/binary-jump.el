;;; -*- lexical-binding: t -*-

(require 'hl-line)

(defvar binary-jump-vertical-range nil)

(defvar binary-jump-round-up t)

(defun binary-jump-select-line (&optional arg)
  (setq binary-jump-vertical-range nil)
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
            (`?p (binary-jump-toward 'up t))
            (`?n (binary-jump-toward 'down t))
            (`?g (setq binary-jump-vertical-range nil))
            (`? (throw 'break nil))
            ((or `? `?  `?j)  (throw 'break t)))
          (when (not (null arg))
            (pop arg)))
      (hl-line-highlight))))

(defun binary-jump-toward (dir &optional continue)
  (pcase dir
    ((or `up `down)
     (let ((beginning-of-visual-line-p (eq (point) (save-excursion (beginning-of-visual-line) (point))))
           (screen-lines-from-top (count-screen-lines (save-excursion (move-to-window-line 0) (point)) (point)))
           (screen-lines-to-bottom (count-screen-lines (point) (save-excursion (move-to-window-line -1) (point)))))
       (unless (and binary-jump-vertical-range continue)
         (setq binary-jump-vertical-range (cons (- screen-lines-from-top 1) (- screen-lines-to-bottom 1))))
       (pcase dir
         (`up (setf (cdr binary-jump-vertical-range) (/ (+ (car binary-jump-vertical-range) (if binary-jump-round-up 1 0)) 2)
                    (car binary-jump-vertical-range) (- (car binary-jump-vertical-range) (cdr binary-jump-vertical-range)))
              (forward-line (- (max (cdr binary-jump-vertical-range) 1)))
              (when (= screen-lines-from-top (if beginning-of-visual-line-p 0 1)) (setq binary-jump-vertical-range nil)))
         (`down (setf (car binary-jump-vertical-range) (/ (+ (cdr binary-jump-vertical-range) (if binary-jump-round-up 1 0)) 2)
                      (cdr binary-jump-vertical-range) (- (cdr binary-jump-vertical-range) (car binary-jump-vertical-range)))
                (forward-line (max (car binary-jump-vertical-range) 1))
                (when (= screen-lines-to-bottom (if beginning-of-visual-line-p 0 1)) (setq binary-jump-vertical-range nil))))))))

(defmacro binary-jump-with-mark-set (&rest body)
  `(let ((mark (point)))
     (if (progn ,@body)
         (unless mark-active (push-mark mark))
       (progn (goto-char mark)))))

(defun binary-jump-previous-line ()
  (interactive)
  (binary-jump-toward 'up (or (eq last-command 'binary-jump-previous-line) (eq last-command 'binary-jump-next-line))))

(defun binary-jump-next-line ()
  (interactive)
  (binary-jump-toward 'down (or (eq last-command 'binary-jump-previous-line) (eq last-command 'binary-jump-next-line))))

(defun binary-jump-select-line-command ()
  (interactive)
  (binary-jump-with-mark-set
   (binary-jump-select-line)))

(defun visual-line-number-at-pos ()
  (interactive)
  (count-screen-lines
   (point-min)
   (save-excursion (beginning-of-visual-line) (point)))) ;这个函数可以计算从buffer开始到光标视觉上的行数

(provide 'binary-jump)
