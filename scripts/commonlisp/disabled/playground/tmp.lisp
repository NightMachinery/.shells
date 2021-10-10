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
;;;
(let ((a "ice")
      (b "sun"))
  (defparameter a a )
  (setf (symbol-value 'b) b)
  (concat a "_" b))

(defmacro letd (bindings &body body)
  `(let ,bindings
     ,@(loop :for b :in bindings
             :for symbol = (if (consp b)
                               (first b)
                               b)
             :for name = (symbol-name symbol)
             :collect `(setf (symbol-value ',(intern (format nil "~A" name))) ,symbol))
     ,@body))

(letd ((a12 "iNA"))
  (concat a12))
;;;
(format t "a~ab" "\\n")
;;;
