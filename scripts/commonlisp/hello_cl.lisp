#!/usr/bin/env -S sbcl --script

(write-line "Hello CL!")

(let ((res
        (run-program "/usr/local/bin/brishzq.zsh" '("ec" "hello") :output *standard-output*)))
  (describe res))

(let* ((res
         (run-program "/usr/local/bin/brishzq.zsh" '("ec" "hello") :output :stream))
       (out-stream (process-output res)))
  (when out-stream
    (loop for line = (read-line out-stream nil)
          while line do (format t "~a~%" line))
    (close out-stream))
  ;; (describe out-stream)
  ;; (describe out)
  ;; (describe res)
  )

(let* (
       (out (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t))
       (err (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t))
       (res
         (with-output-to-string (out-stream out)
           (with-output-to-string (err-stream err)
             (run-program "/usr/local/bin/brishzq.zsh" '("eval" "ec hello && ecerr bad && ec jungle ; bello") :output out-stream :error err-stream))))
       )
  (concatenate 'string (format nil "out: ~a##~%" out)
               (format nil "err: ~a##~%" err))
  )
