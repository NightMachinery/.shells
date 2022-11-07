#!/usr/bin/env -S sbcl_batteriful --script
;; (setq *repl-mode* t)
;;;
(defun main ()
  ;; @note lquery operations are destructive, even serializing to string!
  (letd ((argv (argv-get))
         (url-final-p (or
                       (boolsh
                        (getenv "html_links_url_final_p"))))
         (absolutify-p (or
                       (boolsh-env-y
                        "html_links_absolutify_p")))
         (url_current
          (cond
            (*repl-mode* "http://manpages.ubuntu.com/sth/")
            (t (nth 0 argv))))
         (url_root
          (cond
            (*repl-mode* "http://manpages.ubuntu.com///")
            ((>= (length argv) 2)
             (nth 1 argv))
            (t url_current)))
         (url_root
          (string-right-trim "/" url_root))
         (url_current
          (string-right-trim "/" url_current))
         (in
          (if *repl-mode*
              "<p><a href=\"https://bing.com\">bing!</a>
<b><a href='/posts/internal/1.html'>mm</a></b>
<img src='repo/a.png' />
<img src='//example.com/b.png' />
</p><a href=\"https://google.com\">gingin</a>"

              (alexandria:read-stream-content-into-string *standard-input*)))
         (*doc* (lquery:$ (initialize in))))

    (dolist (attr_name (list "src" "href"))
      (when (and
             url-final-p
             (equalp attr_name "href"))
        (loop for a across
                    (lquery:$ *doc* (inline
                                     (concat "[" attr_name "^=\"http\"]")))
              do
                 (letd ((url
                         (aref (lquery:$ a (attr attr_name)) 0)))
                   (lquery:$ a
                     (attr attr_name
                           (or
                            (empty-str-to-nil
                             (brishz-outrs
                              "reval-memoi" "urlfinalg" url))
                            url))))))

      (when absolutify-p
        (loop for a across
                    (lquery:$ *doc* (inline
                                     ;; (concat "[" attr_name "^=\"/\"]")
;;;
                                     (concat "[" attr_name "]:not([" attr_name "^=\"http\"])")))
              do
                 (letd ((url_rel
                         (aref (lquery:$ a (attr attr_name)) 0)))
                   (lquery:$ a
                     (attr attr_name
                           (concat
                            (cond
                              ((s-starts-with-p "//" url_rel)
                               (elt
                                (nth-value
                                 1
                                 (ppcre:scan-to-strings "^([^/]+:)//"
                                                        url_root))
                                0))
                              ((s-starts-with-p "/" url_rel)
                               url_root)
                              (t
                               (concat url_current "/")))
                            url_rel)))))))

    (when t ;; (not *repl-mode*)
      (format t "~a~%"
              (aref
               (lquery:$ *doc*
                 (serialize))
               0)))))

(main)
