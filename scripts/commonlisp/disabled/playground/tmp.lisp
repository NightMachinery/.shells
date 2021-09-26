(progn (set (intern "FOO") 5)
       (describe foo))
(progn (set (intern "bar") 5)
       (describe bar)                   ;; doesn't work
       )

(defun var-set (var-str val)
  (let ((var-interned
          (intern (string-upcase var-str))))
    (set var-interned val)

    ;; (setf (SYMBOL-VALUE var-interned) val)
    ))

(let ((year "1400"))
  (var-set "year" 1388)
  (labeled identity year))
