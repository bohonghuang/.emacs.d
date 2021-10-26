(require 'parsec)
(require 'dash)
(require 'latex-math-preview)
(require 'calc)

(defcustom calc-trail-tex-result-display-method 'equal
  "Which method should e used for results display.")

(defvar calc-trail-tex-redo-stack '())

(defun calc-trail-tex-parsec-whitespace ()
  (parsec-many (parsec-ch ? )))

(defun calc-trail-tex-parsec-comma ()
  (parsec-ch ?,)
  (calc-trail-tex-parsec-whitespace))

(defun calc-trail-tex-parsec-real-number ()
  (string-to-number (parsec-re "-\\{0,1\\}[0-9]+\\.\\{0,1\\}[0-9]+\\{0,1\\}")))

(defun calc-trail-tex-parsec-complex-number ()
  (parsec-between (parsec-ch ?\() (parsec-ch ?\)) (cons (calc-trail-tex-parsec-number) (parsec-and (calc-trail-tex-parsec-comma) (calc-trail-tex-parsec-whitespace) (calc-trail-tex-parsec-number)))))

(setq calc-trail-tex-operator-alist
      `(("*" . "$1 \\times $2")
        ("/" . "\\frac{$1}{$2}")
        ("+" . "$1 + $2")
        ("-" . "$1 - $2")
        ("sin" . 1)
        ("cos" . 1)
        ("tan" . 1)
        ("()" . "$1 + $2j")
        ("sqrt" . 1)
        ("^" . "{$1}^{$2}")
        ("asin" . "\\arcsin{$1}")
        ("acos" . "\\arccos{$1}")
        ("atan" . "\\arctan{$1}")
        ("abs" . "\\left|$1\\right|")
        ("_" . "-$1")
        ("frac" . "$1")
        ("flt" . "$1")))

(setq calc-trail-tex-operator-priority-alist
      `(("^" . 2)
        ("/" . (,most-negative-fixnum . ,most-positive-fixnum))
        ("sqrt" . (,most-negative-fixnum . 1))
        ("abs" . nil)
        ("*" . -1)
        ("+" . -2)
        ("-" . -2)
        ("_" . ,most-negative-fixnum)
        ("()" . ,most-negative-fixnum)
        ("frac" . (,most-negative-fixnum . ,most-positive-fixnum))
        ("flt" . (,most-negative-fixnum . ,most-positive-fixnum))))

(defun calc-trail-tex-operator-get-priority-out (operator)
  (pcase (assoc operator calc-trail-tex-operator-priority-alist)
    (`nil 0)
    (`(,_ . `nil) most-positive-fixnum)
    (`(,_ . (,prior-in . ,prior-out)) prior-out)
    (`(,_ . ,prior) prior)))

(defun calc-trail-tex-operator-get-priority-in (operator n)
  (pcase (assoc operator calc-trail-tex-operator-priority-alist)
    (`nil 0)
    (`(,_ . nil) most-negative-fixnum)
    (`(,_ . (,prior-in . ,prior-out))
     (pcase prior-in
       (`nil 0)
       ((pred numberp) prior-in)
       ((pred listp) (nth n prior-in))))
    (`(,_ . ,prior) prior)))

(defun calc-trail-tex-parsec-operator-arg ()
  (parsec-ch ?$)
  (string-to-number (parsec-re "[0-9]+")))

(defun calc-trail-tex-parsec-operator-arg-max ()
  (let ((max-number 0))
    (parsec-many (parsec-none-of ?$))
    (parsec-endby (setq max-number (max max-number (calc-trail-tex-parsec-operator-arg))) (parsec-many (parsec-none-of ?$)))
    max-number))

(defun calc-trail-tex-get-operator-consumption (operator)
  (pcase (cdr (assoc operator calc-trail-tex-operator-alist))
    ((and x (pred integerp)) x)
    ((and x (pred stringp)) (parsec-with-input x (calc-trail-tex-parsec-operator-arg-max)))
    (x (error "Undefined operator: %s" operator))))

(defun calc-trail-tex-parsec-number ()
  (parsec-or (parsec-try (calc-trail-tex-parsec-complex-number)) (calc-trail-tex-parsec-real-number)))

(defun calc-trail-tex-operator ()
  (parsec-re "[^0-9][^[:space:]]*"))

(defun calc-trail-tex-element-to-tree (number)
  (pcase number
    (`(,real . ,img) `(("()" . (,(calc-trail-tex-element-to-tree real) ,(calc-trail-tex-element-to-tree img))) . ,number))
   (real (if (>= real 0) real `(("_" . (,(- real))) . ,number)))))

(defun calc-trail-tex-parse-trees (trail)
  (let ((stack '()))
    (parsec-with-input trail
      (while (not (parsec-peek-p (parsec-eof)))
        (pcase (parsec-or (parsec-try (parsec-and (calc-trail-tex-parsec-whitespace) (parsec-return (calc-trail-tex-parsec-number) (parsec-until (parsec-or (parsec-eol-or-eof))))))
                          (parsec-return (parsec-and (calc-trail-tex-parsec-whitespace) (cons (parsec-return (calc-trail-tex-operator) (calc-trail-tex-parsec-whitespace)) (parsec-option nil (calc-trail-tex-parsec-number)))) (parsec-until (parsec-eol-or-eof))))
          ((and x (or (pred numberp) (and `(,real . ,img) (guard (and (numberp real) (numberp img))))))
           (push (calc-trail-tex-element-to-tree x) stack))
          (`("pop" . nil) (pop stack))
          (`("push" . nil) (push (car stack) stack))
          (`(,op . ,res) (let ((args '()))
                                   (dotimes (_ (calc-trail-tex-get-operator-consumption op))
                                     (push (pop stack) args))
                                   (push `((,op . ,args) . ,res) stack)))
          (x (error "Unexpected input: %s" x)))))
    stack))

(defun calc-trail-tex-tree-to-string (tree &optional show-result paren-required)
  (pcase tree
    (`((,op . ,args) . ,res)
     (let* ((op-tex (pcase (cdr (assoc op calc-trail-tex-operator-alist))
                      ((and x (pred integerp))
                       (concat  "\\" op (-reduce #'concat (--map (concat "{$" (number-to-string it) "}") (number-sequence 1 x)))))
                      (x x)))
            (form (-reduce-from (pcase-lambda (acc `(,i . ,x))
                                  (replace-regexp-in-string
                                   (regexp-quote (format "$%d" (+ i 1)))
                                   (pcase x
                                     (`((,op-child . ,args-child) . ,res-child)
                                      (let* ((op-prior (calc-trail-tex-operator-get-priority-in op i))
                                             (op-child-prior (calc-trail-tex-operator-get-priority-out op-child)))
                                        (calc-trail-tex-tree-to-string x
                                                                      (pcase show-result
                                                                        ((and x `underbrace) x)
                                                                        (x nil))
                                                                      (> op-prior op-child-prior))))
                                     (x (calc-trail-tex-tree-to-string x)))
                                   acc
                                   nil
                                   'literal))
                                op-tex
                                (--map-indexed `(,it-index . ,it) args)))
            (form (if paren-required (concat "\\left(" form "\\right)") form)))
       (pcase show-result
         (`nil form)
         (`underbrace (format "\\underbrace{%s}_{%s}" form (calc-trail-tex-tree-to-string res)))
         (`equal (format "%s=%s" form (calc-trail-tex-tree-to-string res))))))
    ((pred numberp) (number-to-string tree))
    (x (let ((form (calc-trail-tex-tree-to-string (calc-trail-tex-element-to-tree x))))
         (if paren-required (concat "\\left(" form "\\right)") form)))))

(defun calc-trail-tex-enter-handler ())

(defun calc-trail-tex-convert-string (trail)
  (concat "\\begin{aligned}" (--reduce (concat acc "\\\\" it) (--map (replace-regexp-in-string "=" "&=" (calc-trail-tex-tree-to-string it calc-trail-tex-result-display-method)) (calc-trail-tex-parse-trees trail))) "\\end{aligned}"))

(defun calc-trail-tex-preview ()
  (interactive)
  (let ((buffer (current-buffer)))
    (with-current-buffer (calc-trail-buffer)
      (let ((buffer-string (buffer-substring-no-properties (point-min) (point-max))))
        (with-temp-buffer
          (insert "$$" (calc-trail-tex-convert-string buffer-string) "$$")
          (backward-char 3)
          (condition-case nil
              (progn (latex-math-preview-expression))
            (error (latex-math-preview-expression))))
        (switch-to-buffer-other-window buffer)))))

(defvar calc-trail-tex-auto-preview-timer nil
  "Idle timer.")

(defvar calc-trail-tex-auto-preview-delay 0.5
  "Idle timer.")

(defun calc-trail-tex-enter-result-handler (&rest _)
  (when calc-trail-tex-auto-preview-timer (cancel-timer calc-trail-tex-auto-preview-timer))
  (setq calc-trail-tex-auto-preview-timer (run-with-idle-timer calc-trail-tex-auto-preview-delay nil (lambda () (calc-trail-tex-preview) (setq calc-trail-tex-auto-preview-timer nil)))))

(defun calc-trail-pop (&optional n)
  (let ((n (or n 1))
        (stack '()))
    (with-current-buffer (calc-trail-buffer)
      (read-only-mode -1)
      (dotimes (i n)
        (end-of-buffer)
        (backward-delete-char 1)
        (let* ((end (point))
              (start (progn (beginning-of-line) (point))))
          (push (buffer-substring-no-properties start end) stack)
          (delete-region start end)))
      (read-only-mode +1)
      stack)))

(defun calc-trail-push (elems)
  (with-current-buffer (calc-trail-buffer)
    (read-only-mode -1)
    (-each elems (lambda (elem)
                   (end-of-buffer)
                   (insert elem "\n")))
    (read-only-mode +1)
    (length elems)))

(defun calc-trail-tex-pop-handler (&rest _)
  (calc-trail-push '(" pop")))

(defun calc-trail-tex-push-handler (&rest _)
  (calc-trail-push '("push")))

(defun calc-trail-tex-undo-handler (&rest _)
  (calc-trail-pop))

(define-minor-mode calc-trail-tex-mode
  "Minor mode for auto refresh calc-trail Tex preview."
  :init-value nil
  (if calc-trail-tex-mode
      (progn (advice-add #'calc-enter-result :after #'calc-trail-tex-enter-result-handler)
             (advice-add #'calc-pop :after #'calc-trail-tex-pop-handler)
             (advice-add #'calc-enter :after #'calc-trail-tex-push-handler)
             (advice-add #'calc-undo :after #'calc-trail-tex-undo-handler))
    (advice-remove #'calc-enter-result #'calc-trail-tex-enter-result-handler)
    (advice-remove #'calc-pop #'calc-trail-tex-pop-handler)
    (advice-remove #'calc-enter #'calc-trail-tex-push-handler)
    (advice-remove #'calc-undo #'calc-trail-tex-undo-handler)))

(define-key calc-mode-map (kbd "<f5>") #'calc-trail-tex-preview)

