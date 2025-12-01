

default: pixelsort.lisp
	sbcl --script $<

load: pixelsort.lisp
	sbcl --load $<
