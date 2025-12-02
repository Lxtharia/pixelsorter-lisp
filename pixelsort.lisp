(load "~/quicklisp/setup.lisp")
(ql:quickload :imago)
(ql:quickload :local-time)
(ql:quickload :clingon)
(use-package 'imago)

; No idea
(defpackage :pixelsortery
  (:use #:imago)
  (:export #:pixelsort))

(load "path-generator")
(load "span-selector")
(load "sorter")
(load "helper")

;;;;;;;;;;;;;;;;;;;;;;
;; pixelsort method ;;
;;;;;;;;;;;;;;;;;;;;;;
;; (in-package :pixelsortery)  ; ?
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

(defun cmdline/handler (cmd)
  (let* ((args (clingon:command-arguments cmd))
	(infile (clingon:getopt cmd :input-file))
	(outfile (clingon:getopt cmd :output-file))
	(first-arg (nth 0 args))
	(second-arg (nth 1 args)))
    ; (format t "~a | ~a ~a ~a ~a ~%" args first-arg second-arg infile outfile)
    ;; use the first two args as infile and outfile
    (if (< 2 (count nil (list first-arg second-arg infile outfile)))
	(error 'arg-error "You need to provide a input and an output file")
	(progn 
	  (setq outfile (or outfile second-arg first-arg))
	  (setq infile  (or infile first-arg))))

    (let* ((start-time (local-time:now))
	   (image (read-image infile))
	   (end-time (local-time:now)))
      (format t "Image loaded in ~a~%" (as-duration-string (timestamp-diff start-time end-time)))
      (let* ((sort-start (local-time:now))
	     (sorted (pixelsort image
		      (make-instance 'line-path)
		      (make-instance 'limit-mark-selector :max 300)
		      (make-instance 'sorting-algo :sort-by 'hue)))
	     (sort-end (local-time:now)))
	(imago:write-image sorted outfile)
	(let* ((write-end (local-time:now))
	       (full-time (timestamp-diff sort-end sort-start))
	       (write-time (timestamp-diff write-end sort-end)))
	  (format t "Sorted ~a pixels in ~a (~a to write).~%"
		  (array-total-size (imago:image-pixels image))
		  (as-duration-string full-time)
		  (as-duration-string write-time)))))))

(defun cmdline/options ()
  "Creates command line options"
  (list
    (clingon:make-option
      :string
      :description "input file"
      :short-name #\i
      :long-name "input"
      :key :input-file)
    (clingon:make-option
      :string
      :description "output file"
      :short-name #\o
      :long-name "output"
      ;; :initial-value "out.png"  ; We need to solve imago being able to convert images
      :key :output-file)))

(defun main ()
  (clingon:run
    (clingon:make-command
      :name "pixelsorterisp"
      :description "A modular pixelsorter"
      :version "0.1.0"
      :license ""
      :authors '("Lxtharia")
      :options (cmdline/options)
      :usage "-i <FILE> [-o <FILE>]"
      :handler #'cmdline/handler
      )))

(main)
