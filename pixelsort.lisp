(ql:quickload :imago)
(use-package 'imago)
; No idea
; (defpackage pixelsorter
;   (:use #:imago)
;   (:export #:pixelsort))



(defun sorting-algo (pixels)
    (sort pixels))

;;; Functional approach
(defun calc-line-path (width height &key horizontal)
  "Returns the pathes for simple lines. Lines are vertical by default."
  (let ((primary (if horizontal width height))
	(secondary (if horizontal height width))
	(primary-size (if horizontal width height)))
    (loop for cross-i below secondary collect
	  (loop for main-i below primary collect
		(if horizontal
		    (cons cross-i main-i)
		    (cons main-i cross-i))))))

(calc-line-path 10 5)

;;; CLOS Approach

;; ;;;;;;;;;;;;;; ;;
;; PATH GENERATOR ;;
(defclass path () ())
(defclass line-path (path) ())

(defgeneric generate-path (path width height)
  (:DOCUMENTATION "Returns one or more lists of coordinates, each list respresenting a path of pixels to sort along."))

(defmethod generate-path ((p line-path) width height)
  (calc-line-path width height))


;; ;;;;;;;;;;;;; ;;
;; SPAN-SELECTOR ;;
(defclass span-selector () ()
  (:DOCUMENTATION "Splits one path of pixels into one or more spans."))

(defclass limit-selector (span-selector)
  ((max-len :initarg :max)
   (random-minimum :initarg :random-min)))

(defclass threshold-selector (span-selector)
  ;; TODO map criteria symbol to criteria fun in constructor
  ((criteria-fun :initarg :criteria :accessor criteria-fun)))

;; Should simply apply limit-selector :around threshold-selector
(defclass limited-threshold-selector (threshold-selector limit-selector) ;; This is important, because only threshold-selector is called on-next-method
  ())

;; Generics 
(defgeneric select-spans (span-selector pixels path))

(defmethod select-spans ((selector threshold-selector) pixels path)
  (let ((spans '())
	(current-span '())) ; TODO: There's gotta be a more functional way
    (loop for (x . y) in path do
      (let ((px (aref pixels x y)))
	(if (< (color-intensity px) 128)
	    (push px current-span)
	    (progn (push current-span spans)
		   (setq current-span (list px))))))))

(defmethod select-spans :around ((selector limit-selector))
  (let ((spans (call-next-method))
	(len 3))
    (loop for i from (length spans) downto 0 by len
	  collect (subseq spans i (+ i len))))) ;; Remove the first element. Placeholder for splitting spans above the limits


;; Testing with tests
(defgeneric test (selector))
(defmethod test ((selector span-selector)) '()) ;; Default impl
(defmethod test ((selector limit-selector))     ;; Returns just the span, because :around already takes care of this
  (print "Cutting paths")
  '(t h i s i s j u s t t h e s p a n))

(defmethod test ((selector threshold-selector))
  (print "Yo, this is threshold selecting 8 stuffs")
  (list 'a 'b 'c 'd 'e 'f 'g 'h))

(defmethod test ((selector limited-threshold-selector))
		 (call-next-method)) ; This simply calls threshold first.

; The magic that gets applied to everything that is or inherits limit-selector
(defmethod test :around ((selector limit-selector))
  (print "Cutting the result of next-method:")
  (subseq (call-next-method) 0 5))

(test (make-instance 'threshold-selector ))

(test (make-instance 'limit-selector))

(test (make-instance 'limited-threshold-selector ))

;;;;;;;;;;;;;;; ;;
;; SORTING-ALGO ;;

;; TODO


(defun pixelsort (image path-generator span-selector sorting-algo)
  (let ((w (image-width image))
	(h (image-height image))
	(pixels (image-pixels image)))
    (imago:make-rgb-image-from-pixels
      (mapcar identity
	      (mapcar (lambda (path)
			(select-spans span-selector pixels path)) ; Returns multiple lists of pixels
		      (mapcar (generate-path path-generator w h))))))) ; returns lists of indices



;;; Main code ;;;


(defparameter *image* (read-image "barbican-london-1.jpg"))

(let ((sorted (pixelsort *image* (make-instance 'line-path) )))
  ())

; (let ((w (imago:image-height *image*)) 
;       (h (imago:image-width *image*)))
;   (format t "~ax~a" w h))

(format t "~a~%" (aref (imago:image-pixels *image*) 1 1))
(imago:color-green (aref (imago:image-pixels *image*) 1 1))

; (loop for px in  (imago:image-pixels *image*) do
	; (format t "~a~%" px))
; (imago:write-image "out.jpg" (pixelsort *image*))

