#!/usr/bin/env -S sbcl_batteriful --script
;;;
;; #!/usr/bin/env -S sbcl --script
;; build:
;;   `in ~/bin html_links_textualize.lisp`
(defparameter *image-dump-mode* nil)
;;;
(defparameter *repl-mode* nil)
;; (setq *repl-mode* t)
;;;
(when *image-dump-mode*
  (let ((init-file (merge-pathnames ".sbclrc"
                                    (user-homedir-pathname))))
    (when (probe-file init-file)
      (progn (load init-file))))
;;;
  (with-output-to-string (*standard-output* nil)
    (ql:quickload "alexandria")
    (ql:quickload :lquery)))
;;;
(defun main ()
  ;; @note lquery operations are destructive, even serializing to string!
  (let* ((in
          (if *repl-mode*
              "<p><a href=\"https://bing.com\">bing!</a></p><a href=\"https://google.com\">gingin</a>"

              (alexandria:read-stream-content-into-string *standard-input*)))
        (*doc* (lquery:$ (initialize in))))

    (loop for a across (lquery:$ *doc*
                         "a")
          do
             (let ((href
                     (aref (lquery:$ a (attr "href")) 0))
                   (text
                     (aref (lquery:$ a (text)) 0)))
               (when
                   (and href (not (string= href ""))
                        text (not (string= text ""))
                        (not (s-starts-with-p "#" href)))
                 (lquery:$ a
                   (text (concat "[[" (org-link-escape href) "][" (org-title-escape text) "]]"))))))

    (format t "~a~%"
            (aref
             (lquery:$ *doc*
               (serialize))
             0))))
;;;
(cond
  (*image-dump-mode*
   (lispexe-save-and-die :name "html_links_textualize.lispexe" :toplevel #'main))
  (t (main)))
