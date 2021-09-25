#!/usr/bin/env -S sbcl --script
;; build:
;;   `in ~/bin tag-filter-date.lisp`
;; test with:
;;   `ec '@futurecron/1/2/3' | tag-filter-date.lispmage`
;;;
(let ((init-file (merge-pathnames ".sbclrc"
                                  (user-homedir-pathname))))
  (when (probe-file init-file)
    (load init-file)))
;;;
(with-output-to-string (*standard-output* nil)
  (ql:quickload "cl-ppcre")
  )

(in-package #:cl-user)
(use-package :cl-ppcre)
;;;
(defparameter *repl-mode* nil)
;; (setq *repl-mode* t)
;;;
(defun tag-filter (&key (stream *standard-input*) (tag "futureCron") (tag-begin #\@) (tag-enter #\/))
  (let*
      ((tag
         (cond
           ((simple-string-p tag)
            `(:SEQUENCE (:FLAGS :CASE-INSENSITIVE-P) ,tag))
           (t tag)))
       (child
         `(:GROUP
           (:SEQUENCE
            (:GREEDY-REPETITION 1 NIL ,tag-enter)
            (:REGISTER
             (:GREEDY-REPETITION 1 NIL (:INVERTED-CHAR-CLASS #\t #\n #\f #\r #\  ,tag-enter))))))
       (child-maybe-1 `(:GREEDY-REPETITION 0 1 ,child))
       (j-year)
       (j-month)
       (j-day))

    (progn ;; setting these at first can help @performance, as the program is waiting idly for pipe input
      (unless j-year
        (setq j-year
              (or
               ;; 1400
               (parse-integer (cmd-result-outrs
                               (brishz-fast "datej-year"))))))
      (unless j-month
        (setq j-month
              (or
               ;; 1
               (parse-integer (cmd-result-outrs
                               (brishz-fast "datej-month"))))))
      (unless j-day
        (setq j-day
              (or
               ;; 1
               (parse-integer (cmd-result-outrs
                               (brishz-fast "datej-day")))))))

    (loop for record = (read-line stream nil)
          while record do
            ;; (format t "rec: ~s" record) ;; @ic

;;; construct the sexp regexes using these:
            ;; (parse-string "[\t\n\f\r ]")
            ;; (parse-string "@((?i)tag)(?:/+([^/]+)){1,3}") ;; @upstreamBug can't handle groups in repeatitions
            ;; (parse-string "@(?:(?i)tag)(?:/+([^/]+))?(?:/+([^/]+))?(?:/+([^/]+))?")
;;;
            (do-register-groups
                (year month day)
                (
                 `(:SEQUENCE ,tag-begin
                             ,tag
                             ,child-maybe-1
                             ,child-maybe-1
                             ,child-maybe-1)
                 record
                 ;; :sharedp t ;; @upstreamBug? "odd number of elements in keyword/value list"
                 )


              ;; (format t "~%rec: ~s~%year: ~s, month; ~s, day: ~s~%" record year month day) ;; @ic

              (when (scan "1\\d{3}" year) ;; shamsi
                (re-mz (var-to-int) (year month day))

                (when
                    (or (< year j-year)
                        (and (= year j-year)
                             (or (< month j-month)
                                 (and (= month j-month)
                                      (<= day j-day)))))
                  (format t "~a~%" record)))))))
;;;
(defun main ()
  (let*
      ((argv (argv-get))
       (tag (elt argv 0)))
    ;; (ec argv) ;; @ic

    (tag-filter
     :tag tag)))

(sb-ext:save-lisp-and-die "tag-filter-date.lispexe" :toplevel #'main :executable t)
