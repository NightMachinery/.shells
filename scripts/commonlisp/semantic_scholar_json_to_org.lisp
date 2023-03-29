#!/usr/bin/env -S sbcl_batteriful --script

(defparameter citations_count_key "citationCount")
(defparameter references_count_key "referenceCount")
(defparameter date_key
  ;; "date"
  "publicationDate"
  )
;; (defparameter year_key "year")
(defparameter authors_names_key "authors_names")
(defparameter topics_key "fieldsOfStudy")

(defparameter in
  (if *repl-mode*
      ""
      (alexandria:read-stream-content-into-string *standard-input*)))

(defparameter d (json-parse in))

(defparameter arxiv-url nil)

;; processes `d' inplace, so that we can again dump its processed form as JSON if need be:
(loop
  ;; `d' is a simple list, with its car specifying its type `:OBJ' in a simple dynamic type checking way
  for kv in (cdr d) do
    (letd ((key (car kv))
           (vs (cdr kv)))
      (setf
       (cdr kv)
       (remove-if
        #'emptyp
        (loop
          for val in vs
          collect
          (let* ((val
                   (cond
                     ((string= key "corpusID")
                      (ppcre:regex-replace-all "\\s*Corpus\\s*ID:\\s*" val ""))
                     ;; ((member key (list
                     ;;               citations_count_key
                     ;;               references_count_key) :test #'string=)
                     ;;  (ppcre:regex-replace-all "\\D+" val ""))
                     ((string= key "links")
                      (when (ppcre:scan "^https?://([^/]+\.)?arxiv.org/" val)
                        (setq arxiv-url
                              (empty-str-to-nil
                               (brishz-outrs "arxiv-url-get" val))))
                      val)
                     (t val)))
                 (val (cond
                        ((stringp val)
                         (pandoc-org-trim-extra
                          (pandoc-normalize-whitespace val)))
                        (t val))))
            (cond
              (t val))))))))

(defun v (key)
  (cond
    ((string= key "arxiv")
     (listify-if-not arxiv-url))
    ((string= key "date_tag")
     (let* ((date (v0 date_key))
            (year
              (or
               (rget "(\\d{4})" date)
               (v0 "year")))
            (month
              (or (rget "([a-zA-Z]+)" date)
                  (month-number-to-name (rget "\\d{4}-(\\d{2})" date)))))
       (listify-if-not
        (when year
          (let ((tag (concat "@" year)))
            (if (and month
                     (not (string= month "")))
                (concat tag "/" month)
                tag))))))
    (t (json-get d key))))

(defun v0 (key)
  (car (v key)))
;;;
(progn
  (let ((title (v0 "title"))
        (url (car (argv-get))))
    (format t "@citations/~a ~a "
            (v0 citations_count_key)
            (v0 "date_tag"))
    (org-link-write :url url :title title)
    (ec))

  (let
      ((out_stream *standard-output*))
    (let ((accessor #'v)
          (keys (list
                 "paperId"
                 "arxiv"
                 date_key
                 citations_count_key
                 "influentialCitationCount"
                 references_count_key
                 "venue"
                 "journal_name"
                 authors_names_key
                 "links"
                 "openAccessPdf"
                 "pdf_urls"
                 "doi"
                 topics_key
                 "isOpenAccess"
                 "corpusID"
                 "year"
                 )))
      (org-properties-write
       :keys keys
       :accessor accessor
       :out_stream out_stream
       :folded_p nil))

    (let ((desc (v0 "abstract"))
          (bibtex (v0 "bibtex")))
      (format out_stream "* @abstract~%")
      (org-properties-write
       :keys nil
       :accessor nil
       :out_stream out_stream
       :folded_p t)
      (org-quote-write :content desc)
      (ec)
      (org-example-write :headers "bibtex" :content bibtex))))
