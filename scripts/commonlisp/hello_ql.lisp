#!/usr/bin/env -S sbcl --noinform --no-userinit --script

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (time (load quicklisp-init))))
