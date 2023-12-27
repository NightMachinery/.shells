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

(defun simple-obj-get (data attr)
  (when data
    (alexandria:when-let*
     ((obj-assoc (assoc :OBJ data))
      (url-assoc (assoc attr (cdr obj-assoc) :test #'equal)))
     (cdr url-assoc))))

(defun v (key)
  (cond
    ((string= key "arxiv")
     (listify-if-not arxiv-url))
    ((string= key "openAccessPdf")
     (simple-obj-get
      (json-get d "openAccessPdf")
      "url"))
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

(defun camel-case-v1 (str)
  (reduce (lambda (acc word) (concatenate 'string acc (string-capitalize word)))
          (split-sequence:split-sequence #\Space str)
          :initial-value ""))

(defun capitalize-first-letter (str)
  (if (zerop (length str))
      ""
      (concatenate 'string (string (char-upcase (elt str 0))) (subseq str 1))))

(defun camel-case (str)
  ;; Split the string on spaces and hyphens using a regex
  (let ((words (cl-ppcre:split "[ -]" str)))
    ;; Capitalize the first letter of each word and concatenate
    (reduce (lambda (acc word) (concatenate 'string acc (capitalize-first-letter word)))
            words
            :initial-value "")))

(defun venue-name-get (str)
  ;; Check for known journals and return their abbreviation
  (cond
    ((cl-ppcre:scan "(?i)^arXiv(?:\.org)?$" str) "arXiv")
    ;; Workshops
    ((cl-ppcre:scan "(?i)IEEE/CVF International Conference on Computer Vision Workshop" str) "ICCVW")
    ((cl-ppcre:scan "(?i)Conference on Computer Vision and Pattern Recognition Workshops" str) "CVPRW")
    ((cl-ppcre:scan "(?i)European Conference on Computer Vision Workshops" str) "ECCVW")
    ((cl-ppcre:scan "(?i)Workshop on Action and Anticipation for Visual Learning" str) "AAVL")
    ((cl-ppcre:scan "(?i)Workshop on Visual Object Tracking" str) "VOT")
    ((cl-ppcre:scan "(?i)Workshop on the Applications of Computer Vision for Drone Technology" str) "ACVDT")
    ((cl-ppcre:scan "(?i)Workshop on Large Scale 3D Data: Acquisition, Modelling and Analysis" str) "LS3DA")
    ((cl-ppcre:scan "(?i)Workshop on Efficient Deep Learning for Computer Vision" str) "EDLCV")
    ((cl-ppcre:scan "(?i)Workshop on Biometrics" str) "BIO")
    ((cl-ppcre:scan "(?i)Workshop on Scene Understanding and Autonomous Systems" str) "SUAS")
    ;; ((cl-ppcre:scan "(?i)Workshop on Applications of Computer Vision" str) "WACV")
    ((cl-ppcre:scan "(?i)International Workshop on Artificial Intelligence for Cultural Heritage" str) "AI4CH")
    ((cl-ppcre:scan "(?i)Workshop on Fairness, Accountability, and Transparency in Machine Learning" str) "FAT/ML")
    ((cl-ppcre:scan "(?i)Workshop on Automated Knowledge Base Construction" str) "AKBC")
    ((cl-ppcre:scan "(?i)Workshop on Machine Learning in Health" str) "ML4H")
    ((cl-ppcre:scan "(?i)Workshop on Computer Vision for Augmented and Virtual Reality" str) "CV4ARVR")
    ((cl-ppcre:scan "(?i)Workshop on Systems for ML" str) "SysML")
    ((cl-ppcre:scan "(?i)Workshop on Robot Learning" str) "CoRL")
    ((cl-ppcre:scan "(?i)Deep Learning for Real-Time Graphics Workshop" str) "DLRTG")
    ((cl-ppcre:scan "(?i)Workshop on Machine Learning for Creativity and Design" str) "ML4AD")
    ((cl-ppcre:scan "(?i)BlackboxNLP Workshop on Analyzing and Interpreting Neural Networks for NLP" str) "BlackboxNLPW")
    ((cl-ppcre:scan "(?i)ACL Workshop" str) "ACLW")
    ;;
    ((cl-ppcre:scan "(?i)Annual Meeting of the Association for Computational Linguistics" str) "ACL")
    ((cl-ppcre:scan "(?i)North American Chapter of the Association for Computational Linguistics" str) "NAACL")
    ((cl-ppcre:scan "(?i)European Chapter of the Association for Computational Linguistics" str) "EACL")
    ((cl-ppcre:scan "(?i)Asian Chapter of the Association for Computational Linguistics" str) "AACL")
    ((cl-ppcre:scan "(?i)Transactions of the Association for Computational Linguistics" str) "TACL")
    ((cl-ppcre:scan "(?i)Conference on Computational Natural Language Learning" str) "CoNLL")
    ((cl-ppcre:scan "(?i)Computational Linguistics" str) "CL")
    ((cl-ppcre:scan "(?i)Journal of Natural Language Engineering" str) "JNLE")
    ((cl-ppcre:scan "(?i)(?:Conference on )?Computer Vision and Pattern Recognition" str) "CVPR")
    ((cl-ppcre:scan "(?i)(?:Conference on )?Uncertainty in Artificial Intelligence" str) "UAI")
    ((cl-ppcre:scan "(?i)(?:Conference on )?Empirical Methods in Natural Language Processing" str) "EMNLP")
    ((cl-ppcre:scan "(?i)International Conference on Computer Vision" str) "ICCV")
    ((cl-ppcre:scan "(?i)European Conference on Computer Vision" str) "ECCV")
    ((cl-ppcre:scan "(?i)Neural Information Processing Systems" str) "NeurIPS")
    ((cl-ppcre:scan "(?i)International Conference on Machine Learning" str) "ICML")
    ((cl-ppcre:scan "(?i)AAAI Conference on Artificial Intelligence" str) "AAAI")
    ((cl-ppcre:scan "(?i)International Joint Conference on Artificial Intelligence" str) "IJCAI")
    ((cl-ppcre:scan "(?i)International Conference on Artificial Intelligence and Statistics" str) "AISTATS")
    ((cl-ppcre:scan "(?i)International Conference on Learning Representations" str) "ICLR")
    ((cl-ppcre:scan "(?i)IEEE International Conference on Robotics and Automation" str) "ICRA")
    ((cl-ppcre:scan "(?i)IEEE/RSJ International Conference on Intelligent Robots and Systems" str) "IROS")
    ((cl-ppcre:scan "(?i)Journal of Machine Learning Research" str) "JMLR")
    ((cl-ppcre:scan "(?i)ACM SIGKDD International Conference on Knowledge Discovery and Data Mining" str) "KDD")
    ((cl-ppcre:scan "(?i)ACM Conference on Information and Knowledge Management" str) "CIKM")
    ((cl-ppcre:scan "(?i)ACM Symposium on Theory of Computing" str) "STOC")
    ((cl-ppcre:scan "(?i)ACM SIGCHI Conference on Human Factors in Computing Systems" str) "CHI")
    ((cl-ppcre:scan "(?i)ACM SIGGRAPH" str) "SIGGRAPH")
    ((cl-ppcre:scan "(?i)ACM Transactions on Graphics" str) "TOG")
    ((cl-ppcre:scan "(?i)ACM SIGCOMM" str) "SIGCOMM")
    ((cl-ppcre:scan "(?i)ACM SIGMOD International Conference on Management of Data" str) "SIGMOD")
    ((cl-ppcre:scan "(?i)ACM International Conference on Supercomputing" str) "ICS")
    ((cl-ppcre:scan "(?i)ACM/IEEE Design Automation Conference" str) "DAC")
    ((cl-ppcre:scan "(?i)ACM/IEEE International Symposium on Computer Architecture" str) "ISCA")
    ((cl-ppcre:scan "(?i)IEEE/CVF Winter Conference on Applications of Computer Vision" str) "WACV")
    ((cl-ppcre:scan "(?i)IEEE Transactions on Pattern Analysis and Machine Intelligence" str) "TPAMI")
    ((cl-ppcre:scan "(?i)IEEE Transactions on Neural Networks and Learning Systems" str) "TNNLS")
    ((cl-ppcre:scan "(?i)IEEE Transactions on Image Processing" str) "TIP")
    ((cl-ppcre:scan "(?i)IEEE Transactions on Robotics" str) "T-RO")
    ((cl-ppcre:scan "(?i)IEEE Transactions on Cybernetics" str) "TCYB")
    ((cl-ppcre:scan "(?i)Pattern Recognition Letters" str) "PRL")
    ((cl-ppcre:scan "(?i)Journal of Artificial Intelligence Research" str) "JAIR")
    ((cl-ppcre:scan "(?i)Journal of Computer Vision and Image Understanding" str) "CVIU")
    ((cl-ppcre:scan "(?i)Knowledge-Based Systems" str) "KBS")
    ((cl-ppcre:scan "(?i)ACM Transactions on Intelligent Systems and Technology" str) "TIST")
    ((cl-ppcre:scan "(?i)Artificial Intelligence Journal" str) "AIJ")
    ((cl-ppcre:scan "(?i)ACM SIGSOFT International Symposium on Software Testing and Analysis" str) "ISSTA")
    ((cl-ppcre:scan "(?i)International Symposium on Software Reliability Engineering" str) "ISSRE")
    ((cl-ppcre:scan "(?i)USENIX Annual Technical Conference" str) "USENIX ATC")
    ((cl-ppcre:scan "(?i)USENIX Symposium on Networked Systems Design and Implementation" str) "NSDI")
    ((cl-ppcre:scan "(?i)USENIX Security Symposium" str) "USENIX Security")
    ((cl-ppcre:scan "(?i)International Symposium on Computer Architecture" str) "ISCA")
    ((cl-ppcre:scan "(?i)IEEE Symposium on Security and Privacy" str) "S&P")
    ((cl-ppcre:scan "(?i)ACM Conference on Embedded Networked Sensor Systems" str) "SenSys")
    ((cl-ppcre:scan "(?i)ACM International Conference on Multimedia" str) "ACM MM")
    ((cl-ppcre:scan "(?i)ACM International Systems and Storage Conference" str) "SYSTOR")
    ((cl-ppcre:scan "(?i)IEEE Transactions on Mobile Computing" str) "TMC")
    ((cl-ppcre:scan "(?i)IEEE Transactions on Knowledge and Data Engineering" str) "TKDE")
    ((cl-ppcre:scan "(?i)ACM Transactions on Information Systems" str) "TOIS")
    ((cl-ppcre:scan "(?i)International Conference on Very Large Data Bases" str) "VLDB")
    ((cl-ppcre:scan "(?i)ACM SIGSOFT Symposium on the Foundations of Software Engineering" str) "FSE")
    ((cl-ppcre:scan "(?i)International Conference on Functional Programming" str) "ICFP")
    ((cl-ppcre:scan "(?i)ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages" str) "POPL")
    ((cl-ppcre:scan "(?i)ACM SIGPLAN Conference on Programming Language Design and Implementation" str) "PLDI")
    ;; Default to camel case for other venue names
    (t (camel-case str))))
;;;
(progn
  (let ((title (v0 "title"))
        (url (car (argv-get)))
        (venue (v0 "venue")))
    (format t "@citations/~a ~a "
            (v0 citations_count_key)
            (v0 "date_tag"))
    (when
        (and venue
             (not (emptyp venue)))
      (format t "@~a "
              (venue-name-get venue)))
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
