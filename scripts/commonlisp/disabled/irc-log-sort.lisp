#!/usr/bin/env -S sbcl --script
;; perf:
;;   `irc-ugb > b` took 22s
;;   `irc-ugb | irc-log-sort.lisp > a` took 18s
;;;
(let ((init-file (merge-pathnames ".sbclrc"
                                  (user-homedir-pathname))))
  (when (probe-file init-file)
    (load init-file)))
;;;
(with-output-to-string (*standard-output* nil)
  (ql:quickload "cl-ppcre")
  (ql:quickload "serapeum")
  (ql:quickload :arrow-macros)
  )

(in-package #:cl-user)
(use-package :cl-ppcre)
(serapeum:toggle-pretty-print-hash-table t)
;;;
(defparameter *repl-mode* nil)
;; (setq *repl-mode* t)
;;;
(defun irc-log-sort (&key (stream *standard-input*)
                       (usernames_self (arrow-macros:->>
                                           (getenv "IRC_USERNAMES")
                                         (split "\\|"))))
  ;; (ec usernames_self)                   ;; @ic

  (let*
      (
       (messages '()))

    (loop for record = (read-line stream nil)
          while record do
            ;; (format t "~%~%rec: ~s~%" record) ;; @ic

            (do-register-groups
                (prefix chat year month day line-number hour minute second sender text)
                (
                 "^([^:]*/+)?([^/]+)/+(\\d{4})-(\\d{2})-(\\d{2})\\.log:(\\d+):\\[(\\d{2}):(\\d{2}):(\\d{2})\\]\\s+<([^>]+)>\\s+(.*)$"
                 record)

              (re-mz (var-to-int _ 1) (year month day line-number hour minute second))
              (setq prefix (or prefix ""))
              (setq text (or text ""))
              (let ((msg (serapeum:dict
                          :prefix prefix
                          :chat chat
                          :year year
                          :month month
                          :day day
                          :line-number line-number
                          :hour hour
                          :minute minute
                          :second second
                          :sender sender
                          :text text)))

                ;; (labeled identity msg) ;; @ic

                (when (not (member sender usernames_self :TEST #'equalp))
                  (setq messages
                        (cons
                         msg
                         messages))))))

    (let* ((messages_sorted
             (sort messages
                   (lambda (a b) ;; t => a before b
                     (loop for prop in '(:year :month :day :hour :minute :second)
                           when (> (gethash prop a)
                                   (gethash prop b))
                             return t
                           when (< (gethash prop a)
                                   (gethash prop b))
                             return nil)))))

      ;; (format t "~%--------------------------------~%")

      (loop for msg in messages_sorted do
        (let ((year (gethash :year msg))
              (month (gethash :month msg))
              (day (gethash :day msg))
              (hour (gethash :hour msg))
              (minute (gethash :minute msg))
              (second (gethash :second msg))
              (line-number (gethash :line-number msg))
              (sender (gethash :sender msg))
              (chat (gethash :chat msg))
              (text (gethash :text msg))
              (prefix (gethash :prefix msg)))
          ;; (format t "~a~%" msg)
          (format t "~a~a/~4,'0d-~2,'0d-~2,'0d.log:~d:[~2,'0d:~2,'0d] <~a> ~a~%" prefix chat year month day line-number hour minute sender text))))))
;;;
(defun main ()
  (let*
      ((argv (argv-get))
       )

    (irc-log-sort)))

(main)
