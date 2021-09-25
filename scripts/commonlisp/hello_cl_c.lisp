#!/usr/bin/env -S sbcl --script

(defun main () (print "hello"))

(sb-ext:save-lisp-and-die "hello_cl_c.lispexe" :toplevel #'main :executable t)
