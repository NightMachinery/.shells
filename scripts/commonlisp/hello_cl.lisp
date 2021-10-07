#!/usr/bin/env -S sbcl --script

(defparameter *repl-mode* nil)
;; (setq *repl-mode* t)
;;;
(let ((init-file (merge-pathnames ".sbclrc"
                                  (user-homedir-pathname))))
  (when (probe-file init-file)
    (load init-file)))
;;;
(write-line "Hello CL!")
