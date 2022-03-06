(require 'cal-iso)

(defun iso-week-to-time (iso-week)
  (pcase-let ((`(,m ,d ,y)
               (calendar-gregorian-from-absolute
                (calendar-iso-to-absolute iso-week))))
    (encode-time 0 0 0 d m y)))

(defun iso-week-to-date (iso-week)      ;week day year
  (calendar-gregorian-from-absolute
   (calendar-iso-to-absolute iso-week)))

(defun iso-week-from-date (date)
  (calendar-iso-from-absolute
   (calendar-absolute-from-gregorian date))) ;week day year

(defun time-to-date (time)
  (let ((decoded-time (decode-time time)))
    (list (nth 4 decoded-time) (nth 3 decoded-time) (nth 5 decoded-time))))

(defun holiday-month-week-day (month week day string)
  (pcase-let* ((`(,iso-week ,iso-day ,iso-year) (iso-week-from-date (list month 1 displayed-year)))
               (date (iso-week-to-date (list (+ iso-week week -1) day iso-year))))
    (when (calendar-date-is-visible-p date)
      (list (list date string)))))

(provide 'calendar-ext)
