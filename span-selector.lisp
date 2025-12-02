; (defpackage :span-selectors)

(defgeneric select-spans (span-selector pixels path)
  (:DOCUMENTATION "Splits the list of coordinates into spans via different criteria and merges them with the values. Returns a list of (coord . value)"))

;;; SPAN-MARK-SELECTOR ;;;
;;;
(defclass span-mark-selector () ()
  (:DOCUMENTATION "Splits one path of pixels into one or more spans. But more extensible."))
;;; Yeah

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
      ; (format t "Marks ~a~%" marked-path)
      spans)))

;; All-selector
(defmethod select-spans ((selector span-mark-selector) pixels path)
  ;; Mark everything as good (2)
  (loop for _ in path collect
	2))

;; Limit selector
(defclass limit-mark-selector (span-mark-selector)
  ((max-len :initarg :max :accessor get-max-len)
   (random-minimum :initarg :random-min)))

(defmethod select-spans ((selector limit-mark-selector) pixels path)
  (loop for i from 0 below (length path)
	collect (if (= 0 (mod i (get-max-len selector))) 1 3)))


;; ;;;;;;;;;;;;; ;;
;; SPAN-SELECTOR ;;
;; Using the simpler, manual span-selector.
;; Methods need the actually implement the span collecting manually
;;
(defclass span-selector () ()
  (:DOCUMENTATION "Splits one path of pixels into one or more spans."))
;;
;; Yeah

(defclass limit-selector (span-selector)
  ((max-len :initarg :max :accessor get-max-len)
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
	(len (get-max-len selector)))
    (loop for i from (length spans) downto 0 by len
	  collect (subseq spans i (+ i len))))) ;; Remove the first element. Placeholder for splitting spans above the limits

