(defvar words-count-rule-chinese "\\cc"
  "A regexp string to match Chinese characters.")

(defvar words-count-rule-nonespace "[^[:space:]]"
  "A regexp string to match none pace characters.")

(defvar words-count-rule-ansci "[A-Za-z0-9][A-Za-z0-9[:punct:]]*"
  "A regexp string to match none pace characters.")

(defvar words-count-regexp-list
  (list words-count-rule-chinese
        words-count-rule-nonespace
        words-count-rule-ansci)
  "A list for the regexp used in `advance-words-count'.")

(defvar words-count-message-func 'message--words-count
  "The function used to format message in `advance-words-count'.")

(defun special--words-count (start end regexp)
  "Count the word from START to END with REGEXP."
  (let ((count 0))
    (save-excursion
      (save-restriction
        (goto-char start)
        (while (and (< (point) end) (re-search-forward regexp end t))
          (setq count (1+ count)))))
    count))

(defun message--words-count (list start end &optional arg)
  "Display the word count message.
Using the LIST passed form `advance-words-count'. START & END are
required to call extra functions, see `count-lines' &
`count-words'. When ARG is specified, display a verbose buffer."
  (message
   (format
    (if arg
        "
-----------~*~ Words Count ~*~----------
 Word Count .................... %d
 Characters (without Space) .... %d
 Characters (all) .............. %d
 Number of Lines ............... %d
 ANSCII Chars .................. %d
%s
========================================
"
      "Wc:%d,Ns:%d,Al:%d,Ln:%d,An:%d,%s")
    (+ (car list) (car (last list)))
    (cadr list)
    (- end start)
    (count-lines start end)
    (car (last list))
    (concat
     (unless (= 0 (car list))
       (format (if arg
                   " Chinese Chars ................. %d\n"
                 "Zh:%d,")
               (car list)))
     (format (if arg
                 " English Words ................. %d\n"
               "En:%d")
             (count-words start end))))))

;;;###autoload
(defun advance-words-count (beg end &optional arg)
  "Chinese user preferred word count.
If BEG = END, count the whole buffer. If called initeractively,
use minibuffer to display the messages. The optional ARG will be
passed to `message--words-count'.

See also `special-words-count'."
  (interactive (if (use-region-p)
                   (list (region-beginning)
                         (region-end)
                         (or current-prefix-arg nil))
                 (list nil nil (or current-prefix-arg nil))))
  (let ((min (or beg (point-min)))
        (max (or end (point-max)))
        list)
    (setq list
          (mapcar
           (lambda (r) (special--words-count min max r))
           words-count-regexp-list))
    (if (called-interactively-p 'any)
        (message--words-count list min max arg)
      list)))
