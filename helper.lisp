
(defun timestamp-diff (start end)
  (if (local-time:timestamp< end start)
      (timestamp-diff end start)
      (local-time:with-decoded-timestamp (:nsec nsec :sec sec :minute min :hour hour :day day :month month :year year) start
	(local-time:adjust-timestamp end
	  (offset :nsec (- nsec))
	  (offset :sec (- sec))
	  (offset :minute (- min))
	  (offset :hour (- hour))
	  ; (offset :day (- 1 day))
	  ; (offset :month (- 1 month))
	  ; (offset :year (- 1 year))
	  ))))

(defun as-duration-string (timestamp)
  (local-time:format-timestring
    nil
    timestamp
    :format '((:min 2) ":" (:sec 2) "." :nsec)))

;; Helper function, written by the computer
(defun flatten-one-level (lst)
  (cond
    ((null lst) nil)  ; Base case: if the list is empty, return nil
    ((listp (car lst))  ; If the first element is a list
     (append (car lst) (flatten-one-level (cdr lst))))  ; Append the first element (flattened) with the rest
    (t (cons (car lst) (flatten-one-level (cdr lst))))))  ; Otherwise, keep the current element and process the rest

