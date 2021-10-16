#!/usr/bin/env -S sbcl_batteriful --script
;; (setq *repl-mode* t)
;;;
(defun main ()
  ;; @note lquery operations are destructive, even serializing to string!
  (letd ((url_base
          (if *repl-mode*
              "http://manpages.ubuntu.com"
              (nth 0 (argv-get))))
         (in
          (if *repl-mode*
              "<p><a href=\"https://bing.com\">bing!</a>
<b><a href='/posts/internal/1.html'>mm</a></b>
<img src='/repo/a.png' />
</p><a href=\"https://google.com\">gingin</a>"

              (alexandria:read-stream-content-into-string *standard-input*)))
         (*doc* (lquery:$ (initialize in))))

    (dolist (attr_name (list "src" "href"))
      (loop for a across
                  (lquery:$ *doc* (inline
                                   ;; (concat "[" attr_name "^=\"/\"]")
;;;
                                   (concat "[" attr_name "]:not([" attr_name "^=\"http\"])")
                                   ))
            do
               (letd ((url_rel
                       (aref (lquery:$ a (attr attr_name)) 0)))
                 (lquery:$ a
                           (attr attr_name (concat url_base "/" url_rel))))))

    (when t ;; (not *repl-mode*)
      (format t "~a~%"
              (aref
               (lquery:$ *doc*
                 (serialize))
               0)))))

(main)
