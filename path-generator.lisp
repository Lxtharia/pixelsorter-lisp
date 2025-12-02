
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

