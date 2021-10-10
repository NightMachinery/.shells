#!/usr/bin/env -S sbcl --script
;; build:
;;   `in ~/bin sbcl_batteriful.lisp`
;;;
(defparameter *repl-mode* nil)
;; (setq *repl-mode* t)
;;;
(let ((init-file (merge-pathnames ".sbclrc"
                                  (user-homedir-pathname))))
  (when (probe-file init-file)
    (progn (load init-file))))
;;;
(dep-load
 "alexandria"
 "str"
 "Serapeum"
 :lquery
 )
;;;
(when (not *repl-mode*)
  (lispexe-save-and-die :name "sbcl_batteriful"))
