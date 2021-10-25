(require 'parsec)

(defun calc-trail-vis-parsec-whitespace ()
  (parsec-many (parsec-ch ? )))

(defun calc-trail-vis-parsec-comma ()
  (parsec-ch ?,)
  (calc-trail-vis-parsec-whitespace))

(defun calc-trail-vis-parsec-real-number ()
  (string-to-number (parsec-re "[0-9]+\\(.[0-9]+\\)\\{0,1\\}")))

(defun calc-trail-vis-parsec-complex-number ()
  (cons (calc-trail-vis-parsec-real-number) (parsec-and (calc-trail-vis-parsec-comma) (calc-trail-vis-parsec-real-number))))

(defvar calc-trail-vis-operator-alist
  '(("*" . "{$2}\\times{$1}")
    ("/" . "\\frac{$2}{$1}")
    ("+" . 2)
    ("-" . 2)
    ("sin" . 1)
    ("cos" . 1)
    ("tan" . 1)
    ("()" . "($2+$1j)")))

(defun calc-trail-vis-parsec-operator-arg ()
  (parsec-ch ?$)
  (string-to-number (parsec-re "[0-9]+")))

(defun calc-trail-vis-parsec-operator-arg-max ()
  (let ((max-number 0))
    (parsec-many (parsec-none-of ?$))
    (parsec-endby (setq max-number (max max-number (calc-trail-vis-parsec-operator-arg))) (parsec-many (parsec-none-of ?$)))
    max-number))

(defun calc-trail-vis-get-operator-consumption (operator)
  (pcase (cdr (assoc operator calc-trail-vis-operator-alist))
    ((and x (guard (integerp x))) x)
    ((and x (guard (stringp x))) (parsec-with-input x (calc-trail-vis-parsec-operator-arg-max)))
    (x (error (format "Undefined operator: %s" x)))))

(defun calc-trail-vis-parsec-number ()
  (parsec-or (parsec-try (calc-trail-vis-parsec-complex-number)) (calc-trail-vis-parsec-real-number)))

(defun calc-trail-vis-operator ()
  (parsec-re "[^0-9][^[:space:]]*"))

(defun calc-trail-vis-parse-tree (trail)
  (let ((stack '()))
    (parsec-with-input trail
      (while (not (parsec-peek-p (parsec-eof))) (pcase (parsec-or (parsec-try (parsec-and (calc-trail-vis-parsec-whitespace) (parsec-return (calc-trail-vis-parsec-number) (parsec-until (parsec-or (parsec-eol-or-eof))))))
                        (parsec-return (parsec-and (calc-trail-vis-parsec-whitespace) (calc-trail-vis-operator)) (parsec-until (parsec-eol-or-eof))))
        ((and x (guard (integerp x))) (push x stack))
        ((and x (guard (stringp x))) (let ((args '()))
                                       (dotimes (_ (calc-trail-vis-get-operator-consumption x))
                                         (push (pop stack) args))
                                       (push (cons x args) stack)))
        (x (error (format "Unexpected input: %s" x))))))
    stack))
