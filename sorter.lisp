
;;;;;;;;;;;;;;; ;;
;; SORTING-ALGO ;;
(defclass sorting-algo () ((criteria :initarg :sort-by)))

(defgeneric sort-pixel-span (sorting-algo image span)
  (:DOCUMENTATION "Sorts the values and puts then back at their coordinates into the provided image. There must be a better way, please"))

(defmethod sort-pixel-span :around ((algo sorting-algo) image span)
  ;; Some more ugly :around magic. Split the spans ( ((x.y) val) ((x.y) val) ...) into coords and vals
  ;; Then sort the vals and set them to the coords in the image
  (let* ((coords (mapcar 'car span))
	 (sorted-values (call-next-method)))
    (dotimes (i (length coords))
      (let ((val (nth i sorted-values)))
	(destructuring-bind (x . y) (nth i coords)
	  (setf (aref image x y ) val))))))


;; built-in sort function
(defmethod sort-pixel-span ((algo sorting-algo) image span)
  (let* ((vals (mapcar 'cdr span)))
    (if (< 2 (length vals))
	(sort vals (lambda (a b) (< a b)))
	vals)))
