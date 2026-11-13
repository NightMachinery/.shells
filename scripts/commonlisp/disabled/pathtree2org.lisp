#!/usr/bin/env -S sbcl_batteriful --script
;;
;; [[id:bed6cce4-3280-42d4-8dbb-ebdf8afc64dc][orgmode/youtube.org:Use =..inline_links..= in the file name to force links to preview]]
;;
;; * @todo9
;; ** Add env var option to follow symlinks
;;;
;; (defpackage #:dummy-package
;;   (:use #:cl #:iterate))
;; (in-package #:dummy-package)
;;;
;; (in-package #:cl-user)
;; (use-package :iterate)
;;;
(defparameter *repl-mode* nil)
;; (setq *repl-mode* t)
;;;
(defun heading (lv text)
  ;; https://stackoverflow.com/questions/24754552/repeat-string-character-with-format
  ;; https://gigamonkeys.com/book/a-few-format-recipes.html (Iteration, Hop)
  (format nil "~v@{~A~:*~} ~*~A~%" lv "*" text)
  )

(comment (heading 3 "Two guns ...")

         (format t "~{~A, ~}" (iterate:iter (iterate:for i from 1 to 10)
                                (iterate:collect "*"))))

(defmacro w (&rest args)
  `(write-string ,args)
  )
;;;
;; (format t "~S" (uiop:command-line-arguments))

(defparameter *dir* (if *repl-mode*
                        "./"
                        (car (uiop:command-line-arguments))))
(defparameter *skip* (if *repl-mode*
                         '()
                         (cdr (uiop:command-line-arguments))))

(w format nil "+TITLE: Pathtree of =~a=~2%" *dir*)

(defun list-dirs (lv dir)
  (when
      (and
       (equalp :DIRECTORY (osicat:file-kind dir
                                            :follow-symlinks nil ;; @buggy following symlinks can cause an infinite loop (by default doesn't follow them)
                                            ))
       (not (symlinkp dir) ;; @workaround
            ))
    (format *error-output* "INFO: Processing ~A ...~2%" dir)
    (let* (
           (fs (cl-fad:list-directory dir :follow-symlinks nil))
           (fs-len (length fs)))

      (iterate:iter (iterate:for f in
                 fs
                 ;; (append (uiop:directory-files dir)
                 ;;         (uiop:subdirectories dir))
                 )
        (iterate:for i from 1)

        (let* ((f-string (namestring f)) ;; '[' is somehow escaped to '\['
               (fn (file-namestring f))  ;; '[' is somehow escaped to '\['
               (fn (if (string= fn "")
                       (first (last (pathname-directory f)))
                       fn))
               (skip-me nil))
          (iterate:iter (iterate:for exc in *skip*)
            (when (ppcre:scan exc f-string)
              (setq skip-me t)))
          (if skip-me
              (format *error-output* "INFO: Skipped ~A~2%" f-string)

              (progn (w heading lv
                        ;; fn
                        (format nil "[[~A][~A]]" (org-link-escape f-string) (org-title-escape fn)))

                     (list-dirs (+ lv 1) f-string)

                     (when (< i fs-len)
                       (format t "~%")))))))))

(when (not *repl-mode*)
  (list-dirs 1 *dir*))
;;;
(comment
 (uiop:directory-exists-p "~/scripts/")
 (osicat:file-kind "/Users/evar/Base/_Code/cpp/cmakeExample0/cmake-build-debug/_deps/mongoose-src/examples/mbed/mongoose")
 (osicat:file-kind "/Users/evar/tmp/akjsk")
 (osicat:file-kind (namestring #P"/Users/evar/base/shared/hypo-diva/Songs/"))
 (describe #P"/Users/evar/base/shared/hypo-diva/Songs/")
 (pathname-type #P"/Users/evar/base/shared/hypo-diva/Songs/")
 (symlinkp #P"/Users/evar/base/shared/hypo-diva/Songs/")
 (symlinkp "/Users/evar/base/shared/dl/")
 (osicat:file-kind "/Users/evar/base/shared/hypo-diva/Songs/" :follow-symlinks nil)
 (equalp :DIRECTORY (osicat:file-kind "/Users/evar/Base/_Code/"))

 (cl-fad:list-directory "/Users/evar/base/shared/hypo-diva" :follow-symlinks nil)
 )
