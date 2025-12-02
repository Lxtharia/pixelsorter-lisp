(load "path-generator")
(load "span-selector")
(load "sorter")

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

(test (make-instance 'threshold-selector ))
(test (make-instance 'limit-selector))
(test (make-instance 'limited-threshold-selector ))
(test (make-instance 'span-mark-selector))
(test (make-instance 'limit-mark-selector))

