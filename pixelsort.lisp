(load "~/quicklisp/setup.lisp")
(ql:quickload :imago)
(use-package 'imago)
; No idea
; (defpackage pixelsorter
;   (:use #:imago)
;   (:export #:pixelsort))




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
(defclass span-selector-plus () ()
  (:DOCUMENTATION "Splits one path of pixels into one or more spans. But more extensible."))

(defclass limit-selector (span-selector)
  ((max-len :initarg :max)
   (random-minimum :initarg :random-min)))

(defclass threshold-selector (span-selector)
  ;; TODO map criteria symbol to criteria fun in constructor
  ((criteria-fun :initarg :criteria :accessor criteria-fun)))

;; Should simply apply limit-selector :around threshold-selector
(defclass limited-threshold-selector (threshold-selector limit-selector) ;; the order is important, because on-next-method should only call threshold-selector
  ())

(defclass masked-thresholder (threshold-selector limit-selector) ;; the order is important, because on-next-method should only call threshold-selector
  ())

;; Generics 
(defgeneric select-spans (span-selector pixels path)
  (:DOCUMENTATION "Splits the list of coordinates into spans via different criteria and merges them with the values. Returns a list of (coord . value)"))

(defmethod select-spans ((selector span-selector-plus) pixels path)
  ;; Mark everything as good (2)
  (loop for _ in path collect
	2))

;; :DOCUMENTATION "Collects the spans using the list of markers.
;; Black excludes a pixel and starts a new span.
;; Gray includes the pixel but starts a new span.")
(defmethod select-spans :around ((selector span-selector-plus) pixels path)
  (let ((spans '())
	(current-span '()) ; TODO: There's gotta be a more functional way
	; Marked path is a list of values 0,1,2
	(marked-path (call-next-method)))
    (flet ((get-pixel-value (coord) (cons coord (aref pixels (car coord) (cdr coord))))
	   (span-break (&optional px)
	     (push current-span spans)
	     (setq current-span px)))
      (loop for i from 0 below (length marked-path) do
	    (let ((mark (nth i marked-path)))
	      ; (format t "~a: ~a, ~a~%" i mark (get-pixel-value (nth i path)))
	      (cond
		((= mark 0) ;; Black
		 (span-break (list (get-pixel-value (nth i path))))
		 (span-break)) ; Fully exclude the pixel
		((= mark 1) ;; Gray
		 (span-break (list (get-pixel-value (nth i path))))) ; Include the pixel in the next span
		(t (push (get-pixel-value (nth i path)) current-span)))))
      (span-break)
      (format t "Selected spans: ~a~%" spans)
      spans)))


; Collect everything
; (defmethod select-spans ((selector span-selector) pixels path)
;   (list (loop for (x . y) in path
; 	      collect (cons (x . y) (aref pixels x y)))))
;
; (defmethod select-spans ((selector threshold-selector) pixels path)
;   (let ((spans '())
; 	(current-span '())) ; TODO: There's gotta be a more functional way
;     (loop for (x . y) in path do
;       (let ((px (aref pixels x y)))
; 	(if (< (color-intensity px) 128)
; 	    (push px current-span)
; 	    (progn (push current-span spans)
; 		   (setq current-span (list px))))))))
;
; (defmethod select-spans ((selector limit-selector) pixels path))
;
; (defmethod select-spans :around ((selector limit-selector) pixels path)
;   (let ((spans (call-next-method))
; 	(len 3))
;     (loop for i from (length spans) downto 0 by len
; 	  collect (subseq spans i (+ i len))))) ;; Remove the first element. Placeholder for splitting spans above the limits

;;;;;;;;;;;;;;; ;;
;; SORTING-ALGO ;;

(defclass sorting-algo () ((criteria :initarg :sort-by)))
(defgeneric sort-pixel-span (sorting-algo image span)
  (:DOCUMENTATION "Sorts the values and puts then back at their coordinates into the provided image. There must be a better way, please"))

(defmethod sort-pixel-span :around ((algo sorting-algo) image span)
  (let ((sorted-values (call-next-method)))
    (loop for ((x . y) . val) in sorted-values do
	  (format t ">> ~a ~a ~a~%" x y val)
	  (setf (aref image x y) val))))

(defmethod sort-pixel-span ((algo sorting-algo) image span)
  (if (> 2 (length span))
      (sort span (lambda (a b) (< (cdr a) (cdr b))))
      span))


;;;;;;;;;;;;;;;;;;;;;;
;; pixelsort method ;;


;; Thanks AI
(defun flatten-one-level (lst)
  (cond
    ((null lst) nil)  ; Base case: if the list is empty, return nil
    ((listp (car lst))  ; If the first element is a list
     (append (car lst) (flatten-one-level (cdr lst))))  ; Append the first element (flattened) with the rest
    (t (cons (car lst) (flatten-one-level (cdr lst))))))  ; Otherwise, keep the current element and process the rest


(defun pixelsort (image path-generator span-selector sorting-algo)
  (let* ((w (image-width image))
	 (h (image-height image))
	 (pixels (image-pixels image))
	 (paths (generate-path path-generator w h))
	 (new-image (imago:make-rgb-image w h)))
    (format t "Sorting the path ~a~%" paths)
    (mapcar (lambda (s) (format t "Sorting span: ~a~%" s) (sort-pixel-span sorting-algo (image-pixels new-image) s))
	    (flatten-one-level
	      (mapcar (lambda (path) (select-spans span-selector pixels path)) ; Returns a lists of spans for each path
		      paths)))
    new-image))


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

; (test (make-instance 'threshold-selector ))
; (test (make-instance 'limit-selector))
; (test (make-instance 'limited-threshold-selector ))
; (test (make-instance 'span-selector-plus))


;;; Main code ;;;

; (defparameter *image* (read-image "./barbican-london-1.jpg"))
(defparameter *image* (read-image "pixeltest.png"))

(let* ((sorted (pixelsort *image*
			  (make-instance 'line-path)
			  (make-instance 'span-selector-plus)
			  (make-instance 'sorting-algo :sort-by 'hue))))
  (imago:write-image sorted "out.png"))

(format t "~a~%" (aref (imago:image-pixels *image*) 1 1))
(imago:color-green (aref (imago:image-pixels *image*) 1 1))

; (loop for px in  (imago:image-pixels *image*) do
	; (format t "~a~%" px))
; (imago:write-image "out.jpg" (pixelsort *image*))

