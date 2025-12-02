(load "~/quicklisp/setup.lisp")
(ql:quickload :imago)
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

(defparameter *image* (read-image "./barbican-london-1.jpg"))

(let* ((sorted (pixelsort *image*
			  (make-instance 'line-path)
			  (make-instance 'limit-mark-selector :max 300)
			  (make-instance 'sorting-algo :sort-by 'hue))))
  (imago:write-image sorted "out.jpg"))

(format t "Sorted ~a pixels.~%" (array-total-size (imago:image-pixels *image*)))


