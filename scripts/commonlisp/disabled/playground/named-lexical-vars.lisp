(defvar *lexical-variables* '())

(defun symbolify (name)
  (cond
    ((symbolp name) name)
    (t (intern (string-upcase name)))))

(defun get-var (name)
  (let (
        (var (cdr (assoc
                   (symbolify name)
                   *lexical-variables*))))
    (unless var (error "No lexical variable named ~S" name))
    var))

(defun lexical-var-deref (var)
  (funcall (if (symbolp var)
               (or (cdr (assoc var *lexical-variables*))
                   (error "No lexical variable named ~S" var))
               var)))

(defun (setf lexical-var-deref) (new-value var)
  (funcall (if (symbolp var)
               (or (cdr (assoc var *lexical-variables*))
                   (error "No lexical variable named ~S" var))
               var)
           new-value))

(defmacro with-named-lexical-variable ((&rest vars) &body body)
  (let ((vvar (gensym))
        (vnew-value (gensym))
        (vsetp (gensym)))
    `(let ((*lexical-variables*
             ;; list* is like list except that the last argument to list becomes the car of the last cons constructed, while the last argument to list* becomes the cdr of the last cons constructed. If the last argument to list* is a list, the effect is to construct a new list which is similar, but which has additional elements added to the front corresponding to the preceding arguments of list*.
             (list* ,@(mapcar (lambda (var)
                                `(cons ',var
                                       (lambda (&optional (,vnew-value nil ,vsetp))
                                         (if ,vsetp
                                             (setf ,var ,vnew-value)
                                             ,var))))
                              vars)
                    *lexical-variables*)))
       ,@body)))


(defun var-set (var-str val)
  (let ((var-interned (symbolify var-str)))
    (setf (lexical-var-deref var-interned) val)))

;;;
(let ((x 1)
      (y 2))
  (with-named-lexical-variable (x y)
    (var-set "x" 3)
    (setf (lexical-var-deref 'y) 4)
    (format t "inside: ~a~%" (mapcar (function lexical-var-deref) '(x y))))
  (format t "outside: x: ~s y: ~s~%" x y))
;; -> (3 4)


(let ((year "1400"))
  (with-named-lexical-variable (year)
    (var-set "year" 1388))
  year)
;; --> 1388
;;;

(defun foo (a b)
  (let ((x (+ a 1))
        (y (+ b 2)))
    (with-named-lexical-variable (a b x y)
      (loop for var in '("x" "y" "a" "b")
            for i from 10
            do (var-set var i))
      (setf (lexical-var-deref 'y) 4)
      (mapcar (function lexical-var-deref) '(x y a b p q)))))

(defun bar (p q)
  (with-named-lexical-variable (p q)
    (foo p q)))

(bar 1 2)

;; --> (10 4 12 13 1 2)


(defun foo* (*a *b)
  (let ((x (+ (lexical-var-deref *a) 1))
        (y (+ (lexical-var-deref *b) 2)))
    (with-named-lexical-variable (x y *a *b)
      (loop for var in '("x" "y")
            for i from 10
            do (var-set var i))
      (loop for var in '("*a" "*b")
            for i from 20
            do (setf (lexical-var-deref
                      (funcall (get-var var)) ;; this dereference will now yield the original input to foo*, which was itself a lambda-boxed var
                      ) i))
      (setf (lexical-var-deref 'y) 4)
      (mapcar (function lexical-var-deref) (list 'x 'y *a *b 'p 'q)))))


(defun bar* (p q)
  (with-named-lexical-variable (p q)
    (foo* (get-var 'p) (get-var 'q))))


(bar* 1 2)
;; --> (10 4 20 21 20 21)
;;;
