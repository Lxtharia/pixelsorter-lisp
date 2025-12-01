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
(defgeneric select-spans (span-selector pixels path)
  (:DOCUMENTATION "Splits the list of coordinates into spans via different criteria and merges them with the values. Returns a list of (coord . value)"))

; Two main classes
(defclass span-selector () ()
  (:DOCUMENTATION "Splits one path of pixels into one or more spans."))
(defclass span-mark-selector () ()
  (:DOCUMENTATION "Splits one path of pixels into one or more spans. But more extensible."))

;; Select Methods
;; Advanced :around method for selecting spans
;; The primary method only has to return a linear list of marks with the same length as the path.
;; Meaning of marks:
;; 0 (Black):    exclude pixel and start a new span
;; 1 (Gray):     start a new span but include pixel
;; else (White): collect pixel to the current-span
(defmethod select-spans :around ((selector span-mark-selector) pixels path)
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
      (format t "Marks ~a~%" (first marked-path))
      spans)))

;; TODO: this is called on limit-mark-selector instance
;; All-selector
(defmethod select-spans ((selector span-mark-selector) pixels path)
  ;; Mark everything as good (3)
  (loop for _ in path collect
	3))

;; Limit selector
(defclass limit-mark-selector (span-mark-selector) ())
(defmethod select-span ((selector limit-mark-selector) pixels path)
  (loop for _ in path collect
	0))
	;  (loop for i from 0 below (length path)
	; collect (if (= 0 (% i 50)) 1 0)))


;;
;; Using the simpler, manual span-selector.
;; Methods need the actually implement the span collecting manually
;; Yeah
;;

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

(defmethod select-spans ((selector span-selector) pixels path)
  (list (loop for (x . y) in path
	      collect (cons (cons x y) (aref pixels x y)))))
;
(defmethod select-spans ((selector threshold-selector) pixels path)
  (let ((spans '())
	(current-span '())) ; TODO: There's gotta be a more functional way
    (loop for (x . y) in path do
      (let ((px (aref pixels x y)))
	(if (< (color-intensity px) 128)
	    (push (cons (cons x y) px) current-span)
	    (progn (push current-span spans)
		   (setq current-span (list (cons (cons x y) px)))))))))

(defmethod select-spans ((selector limit-selector) pixels path))

(defmethod select-spans :around ((selector limit-selector) pixels path)
  (let ((spans (call-next-method))
	(len 3))
    (loop for i from (length spans) downto 0 by len
	  collect (subseq spans i (+ i len))))) ;; Remove the first element. Placeholder for splitting spans above the limits

;;;;;;;;;;;;;;; ;;
;; SORTING-ALGO ;;

(defclass sorting-algo () ((criteria :initarg :sort-by)))
(defgeneric sort-pixel-span (sorting-algo image span)
  (:DOCUMENTATION "Sorts the values and puts then back at their coordinates into the provided image. There must be a better way, please"))

(defmethod sort-pixel-span :around ((algo sorting-algo) image span)
  ;; Some more ugly around magic. Split the spans ( ((x.y) val) ((x.y) val) ...) into coords and vals
  ;; Then sort the vals and set them to the coords in the image
  (let* ((coords (mapcar 'car span))
	 (sorted-values (call-next-method)))
    (dotimes (i (length coords))
      (let ((val (nth i sorted-values)))
	(destructuring-bind (x . y) (nth i coords)
	  (setf (aref image x y ) val))))))

(defmethod sort-pixel-span ((algo sorting-algo) image span)
  (let* ((vals (mapcar 'cdr span)))
    (if (< 2 (length vals))
	(sort vals (lambda (a b) (< a b)))
	vals)))


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
    (mapcar (lambda (s) (sort-pixel-span sorting-algo (image-pixels new-image) s))
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
(defmethod test :around ((selector span-mark-selector))
  (print "Around spanmark>")
  (format t "Around: ~a~%" (call-next-method))
  (print "<Around spanmark"))

(defmethod test ((selector span-mark-selector))
  (print "Primary1")
  (list 1 1 1))

(defmethod test ((selector limit-mark-selector))
  (print "Primary2")
  (list 2 2 2))

; (test (make-instance 'threshold-selector ))
; (test (make-instance 'limit-selector))
; (test (make-instance 'limited-threshold-selector ))
(test (make-instance 'span-mark-selector))
(test (make-instance 'limit-mark-selector))


;;; Main code ;;;

; (defparameter *image* (read-image "./barbican-london-1.jpg"))
(defparameter *image* (read-image "./barbican-london-1.jpg"))

(let* ((sorted (pixelsort *image*
			  (make-instance 'line-path)
			  (make-instance 'limit-mark-selector)
			  (make-instance 'sorting-algo :sort-by 'hue))))
  (imago:write-image sorted "out.jpg"))

(format t "Sorted ~a~%" (aref (imago:image-pixels *image*) 1 1))
(imago:color-green (aref (imago:image-pixels *image*) 1 1))

; (loop for px in  (imago:image-pixels *image*) do
	; (format t "~a~%" px))
; (imago:write-image "out.jpg" (pixelsort *image*))

