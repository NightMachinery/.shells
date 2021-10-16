#!/usr/bin/env -S sbcl_batteriful --script

(defparameter in
  (if *repl-mode*
      "{
  \"description\": [
    \"*Strategies for building large systems that can be easily adapted for new situations with only minor programming modifications.*\\\\\\\\\\n\\\\\\\\\\nTime pressures encourage programmers to write code that works well for a narrow purpose, with no room to grow. But the best systems are evolvable; they can be adapted for new situations by adding code, rather than changing the existing code. The authors describe techniques they have found effective--over their combined 100-plus years of programming experience--that will help programmers avoid programming themselves into corners.\\\\\\\\\\n\\\\\\\\\\nThe authors explore ways to enhance flexibility by:\\\\\\\\\\n*-* Organizing systems using combinators to compose mix-and-match parts, ranging from small functions to whole arithmetics, with standardized interfaces\\\\\\\\\\n*-* Augmenting data with independent annotation layers, such as units of measurement or provenance\\\\\\\\\\n*-* Combining independent pieces of partial information using unification or propagation\\\\\\\\\\n*-* Separating control structure from problem domain with domain models, rule systems and pattern matching, propagation, and dependency-directed backtracking\\\\\\\\\\n*-* Extending the programming language, using dynamically extensible evaluators\"
  ],
  \"bookFormat\": [
    \"Hardcover\"
  ],
  \"pageCount\": [
    \"448 pages\"
  ],
  \"publicationDetails\": [
    \"Published March 9th 2021 by MIT Press\"
  ],
  \"title\": [
    \"Software Design for Flexibility: How to Avoid Programming Yourself Into a Corner\"
  ],
  \"authors\": [
    \"<<bookAuthors>>\\nby\\n\\n[[https://www.goodreads.com/author/show/8091654.Chris_Hanson][Chris Hanson]],\\n\\n[[https://www.goodreads.com/author/show/24589.Gerald_Jay_Sussman][Gerald Jay Sussman]]\"
  ],
  \"rating\": [
    \"3.06\"
  ],
  \"ratingCount\": [
    \"16\"
  ],
  \"reviewCount\": [
    \"4\"
  ],
  \"isbn\": [
    \"9780262045490\"
  ],
  \"language\": [],
  \"series\": [],
  \"genres\": [
    \"[[https://www.goodreads.com/genres/computer-science][Computer Science]]\",
    \"[[https://www.goodreads.com/genres/programming][Programming]]\",
    \"[[https://www.goodreads.com/genres/computer-science][Computer Science]]\",
    \"[[https://www.goodreads.com/genres/software][Software]]\",
    \"[[https://www.goodreads.com/genres/science][Science]]\",
    \"[[https://www.goodreads.com/genres/computer-science][Computer Science]]\",
    \"[[https://www.goodreads.com/genres/computer-science][Computer Science]]\",
    \"[[https://www.goodreads.com/genres/technical][Technical]]\",
    \"[[https://www.goodreads.com/genres/science][Science]]\",
    \"[[https://www.goodreads.com/genres/technology][Technology]]\",
    \"[[https://www.goodreads.com/genres/non-fiction][Nonfiction]]\",
    \"[[https://www.goodreads.com/genres/computer-science][Computer Science]]\",
    \"[[https://www.goodreads.com/genres/computers][Computers]]\"
  ]
}
"
      (alexandria:read-stream-content-into-string *standard-input*)))

(defparameter d (json-parse in))

;; processes `d' inplace, so that we can again dump its processed form as JSON if need be:
(loop
  ;; `d' is a simple list, with its car specifying its type `:OBJ' in a simple dynamic type checking way
  for kv in (cdr d) do
    (letd ((key (car kv))
           (vs (cdr kv)))
      (setf
       (cdr kv)
       (loop
         for val in vs
         collect
         (let* ((val (pandoc-org-trim-extra
                      (pandoc-normalize-whitespace val))))
           (cond
             ((string= key "title")
              (string-left-trim "* " val))
             ((string= key "authors")
              (let* ((val (ppcre:regex-replace-all "\\n+" val " "))
                     (val (ppcre:regex-replace-all "\\bby\\b" val ""))
                     (val (string-trim " " val)))
                val))
             ((or
               (string= key "description")
               (string= key "descriptionShort"))
              (let* ((val (org-trim-forced-newlines val))
                     (val (ppcre:regex-replace-all "(^|(?<=\\n))\\*-\\* " val "- "))
                     ;; (val (string-trim " ()" val))
                     )
                val))
             (t val)))))))

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
        (keys '("authors"
                "rating"
                "ratingCount"
                "reviewCount"
                "series"
                ;; "bookFormat"
                "pageCount"
                "publicationDetails"
                "genres"
                "isbn"
                )))
    (org-properties-write :keys keys :accessor accessor :out_stream out_stream))

  (let* ((desc (v0 "description"))
        (desc
          (cond
            ((not (empty-str-to-nil desc))
             (v0 "descriptionShort"))
            (t desc))))
    (org-quote-write :content desc)
    ))
