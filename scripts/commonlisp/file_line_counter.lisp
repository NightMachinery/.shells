#!/usr/bin/env -S sbcl_batteriful --script

(defparameter in
  (if *repl-mode*
      "/Users/evar/notes/bookmarks/play/videogames/2d fighting.org
/Users/evar/notes/bookmarks/play/videogames/9p.org
"
      (alexandria:read-stream-content-into-string *standard-input*)))

(setf lparallel:*kernel* (lparallel:make-kernel 16 :name "custom-kernel"))

(defun file-line-counter (files)
  (letd ((all-lines 0)
         (files-augmented
          (lparallel:pmap
           'vector
           (lambda (file)
             (letd ((file-path
                     (make-pathname :directory "" :name file)) ;; Needed for escaping the glob (wild) characters
                    (file-str (alexandria::read-file-into-byte-vector file-path))
                    (line-count (alexandria::count
                                 (char-int #\Newline)
                                 file-str)))
               (list file line-count)))
           files
           ))
         (line-count-extractor
          (lambda (x) (cadr x)))
         (all-lines (reduce
                     #'+
                     files-augmented
                     :key line-count-extractor
                     :initial-value all-lines)))
    (sort files-augmented #'>
          :key line-count-extractor)
    (format t "All lines: ~d~%" all-lines)
    (loop for f across files-augmented do
      (letd ((line-count (cadr f))
             (file-name (car f)))
        (format t "~d: ~a~%" line-count file-name)))))

(file-line-counter (ppcre:split #\Newline in))

(lparallel:end-kernel :wait t)
