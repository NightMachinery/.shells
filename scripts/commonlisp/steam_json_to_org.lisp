#!/usr/bin/env -S sbcl_batteriful --script

(defparameter in
  (if *repl-mode*
      ""
      (alexandria:read-stream-content-into-string *standard-input*)))

(defparameter d (json-parse in))

;; processes `d' inplace, so that we can again dump its processed form as JSON if need be:
(loop
  ;; `d' is a simple list, with its car specifying its type `:OBJ' in a simple dynamic type checking way
  for kv in (cdr d) do
    (letd ((key (car kv))
           (vs (cdr kv))
           (platforms_seen nil))
      (setf
       (cdr kv)
       (remove-if
        #'emptyp
        (loop
          for val in vs
          collect
          (let* ((val
                   (cond
                     ((string= key "platforms")
                      (letd ((platform (cadr val))
                             (platform (string-downcase platform)))
                        (if
                         (and
                          (member
                           platform
                           (list "win" "mac" "linux") :test #'string=)
                          (not
                           (member
                            platform
                            platforms_seen
                            :test #'string=)))
                         (progn
                           (setq platforms_seen
                                 (cons platform platforms_seen))
                           platform)
                         "")))
                     (t val)))
                 (val (pandoc-org-trim-extra
                       (pandoc-normalize-whitespace val))))
            (cond
              ((uiop:string-suffix-p key "Reqs")
               (let* ((val (org-trim-forced-newlines val))
                      (val (ppcre:regex-replace-all "\\n+(?:-\\s*)?" val " |> "))
                      (val
                        (string-trim
                         (list #\Space #\Newline)
                         val))
                      )
                 val)
               )
              ((string= key "tags")
               (when (not (string= val "+"))
                 val))
              ((string= key "description")
               (let* ((val (org-trim-forced-newlines val))
                      (val
                        (string-trim
                         (list #\Space #\Newline)
                         (ppcre:regex-replace-all "\\*+ About.*" val "")))
                      )
                 val))
              (t val))))))))

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
        (keys '("releaseDate"
                "metacriticScore"
                "allReviews"
                "recentReviews"
                "tags"
                "developers"
                "publisher"
                "platforms"
                "capabilities"
                "minReqs"
                "recommendedReqs"
                "metacriticUrl"
                "img")))
    (org-properties-write :keys keys :accessor accessor :out_stream out_stream))

  (let ((desc (v0 "description")))
    (org-quote-write :content desc)
    ))
