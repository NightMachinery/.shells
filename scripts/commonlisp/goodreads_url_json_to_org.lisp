#!/usr/bin/env -S sbcl --script

(defparameter repl-mode nil)
;; (setq repl-mode t)
;;;
(let ((init-file (merge-pathnames ".sbclrc"
                                  (user-homedir-pathname))))
  (when (probe-file init-file)
    (load init-file)))
;;;
(with-output-to-string (*standard-output* nil)
  (ql:quickload "alexandria"))

(defparameter in (alexandria:read-stream-content-into-string *standard-input*))

(defparameter d (json-parse in))
(defun v (key)
  (json-get d key))

(defun v0 (key)
  (car (v key)))
;;;
(progn
  (let ((title (v0 "title"))
        (url (car (argv-get))))
    (org-link-write :url url :title title)
    (ec))

  (let ((out_stream *standard-output*)
        (accessor #'v)
        (keys '("series"
                ;; "bookFormat"
                "pageCount"
                "publicationDetails"
                "genres"
                "isbn"
                )))
    (org-properties-write :keys keys :accessor accessor :out_stream out_stream))

  (let ((desc (v "description")))
    (org-quote-write :content desc)
    ))
