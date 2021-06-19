#!/usr/bin/env -S sbcl --script

(let ((init-file (merge-pathnames ".sbclrc"
                                       (user-homedir-pathname))))
  (when (probe-file init-file)
    (load init-file)))

(with-output-to-string (*standard-output* nil)
  (ql:quickload "alexandria"))
;;;
(format t "~S~%" (alexandria:read-stream-content-into-string *standard-input*))
