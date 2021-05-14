#!/usr/bin/env -S sbcl --script

(defun main () (print "hello"))

(sb-ext:save-lisp-and-die "hello_cl_c.exe" :toplevel #'main :executable t)
