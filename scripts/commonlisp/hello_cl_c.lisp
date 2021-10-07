#!/usr/bin/env -S sbcl --script

(defun main () (print "hello"))

;; @warn use 'lispexe-save-and-die' instead when our functions are loaded
(sb-ext:save-lisp-and-die "hello_cl_c.lispexe" :toplevel #'main :executable t)
