#!/usr/bin/env -S sbcl_batteriful --script

(format t "~S~%" (alexandria:read-stream-content-into-string *standard-input*))
