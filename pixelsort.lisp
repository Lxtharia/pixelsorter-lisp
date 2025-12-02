(load "~/quicklisp/setup.lisp")
(ql:quickload :imago)
(ql:quickload "local-time")
(use-package 'imago)

; No idea
(defpackage :pixelsortery
  (:use #:imago)
  (:export #:pixelsort))

(load "path-generator")
(load "span-selector")
(load "sorter")

;; Helper function, written by the computer
(defun flatten-one-level (lst)
  (cond
    ((null lst) nil)  ; Base case: if the list is empty, return nil
    ((listp (car lst))  ; If the first element is a list
     (append (car lst) (flatten-one-level (cdr lst))))  ; Append the first element (flattened) with the rest
    (t (cons (car lst) (flatten-one-level (cdr lst))))))  ; Otherwise, keep the current element and process the rest

;;;;;;;;;;;;;;;;;;;;;;
;; pixelsort method ;;
;;;;;;;;;;;;;;;;;;;;;;

; (in-package :pixelsortery) 
(defun pixelsort (image path-generator span-selector sorting-algo)
  (let* ((w (image-width image))
	 (h (image-height image))
	 (pixels (image-pixels image))
	 (paths (generate-path path-generator w h))
	 (new-image (imago:make-rgb-image w h)))
    (mapcar (lambda (s) (sort-pixel-span sorting-algo (image-pixels new-image) s))
	    (flatten-one-level
	      (mapcar (lambda (path) (select-spans span-selector pixels path)) ; Returns a lists of spans for each path
		      paths)))
    new-image))


;;;;;;;;;;;;;;;;;
;;; Main code ;;;
;;;;;;;;;;;;;;;;;

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

(defparameter *image-path* "./images/barbican-london.jpg")
(defparameter *out-path* "out.jpg")

(let* ((start-time (local-time:now))
       (image (read-image *image-path*))
       (end-time (local-time:now)))
  (format t "Image loaded in ~a~%" (as-duration-string (timestamp-diff start-time end-time)))
  (let* ((sort-start (local-time:now))
	 (sorted (pixelsort image
		  (make-instance 'line-path)
		  (make-instance 'limit-mark-selector :max 300)
		  (make-instance 'sorting-algo :sort-by 'hue)))
	 (sort-end (local-time:now)))
    (imago:write-image sorted *out-path*)
    (let* ((write-end (local-time:now))
	   (full-time (timestamp-diff sort-end sort-start))
	   (write-time (timestamp-diff write-end sort-end)))
      (format t "Sorted ~a pixels in ~a (~a to write).~%"
	      (array-total-size (imago:image-pixels image))
	      (as-duration-string full-time)
	      (as-duration-string write-time)))))


