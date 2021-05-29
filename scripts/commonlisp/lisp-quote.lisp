#!/usr/bin/env -S sbcl --script

(loop for line = (read-line *standard-input* nil)
          while line do (format t "~S~%" line))
