(ql:quickload :imago)
(use-package 'imago)
; No idea
; (defpackage pixelsorter
;   (:use #:imago)
;   (:export #:pixelsort))



(defun sorting-algo (pixels)
    (sort pixels))



(defun line-path (width height &key horizontal)
  "Returns the pathes for simple lines. Lines are vertical by default."
  (let ((primary (if horizontal width height))
	(secondary (if horizontal height width))
	(primary-size (if horizontal width height)))
    (loop for cross-i below secondary collect
	  (loop for main-i below primary collect
		(+ (* cross-i primary-size) main-i)))))

(line-path 10 5)
;; Functional approach
;; No classes :(

(defun pixelsort (image path-traverser span-selector sorting-algo)
  (let ((w (image-width image)) (h (image-height image))
    (imago:make-rgb-image-from-pixels
     (mapcar sorting-algo
	     (mapcar (lambda (path) (select span-selector path image)) ; Returns multiple lists of pixels
	      (mapcar path-traverser w h))))))) ; returns lists of indices



;;; Main code ;;;


(defparameter *image* (read-image "barbican-london-1.jpg"))

; (let ((w (imago:image-height *image*)) 
;       (h (imago:image-width *image*)))
;   (format t "~ax~a" w h))

(format t "~a~%" (aref (imago:image-pixels *image*) 1 1))
(imago:color-green (aref (imago:image-pixels *image*) 1 1))

; (loop for px in  (imago:image-pixels *image*) do
	; (format t "~a~%" px))
; (imago:write-image "out.jpg" (pixelsort *image*))

