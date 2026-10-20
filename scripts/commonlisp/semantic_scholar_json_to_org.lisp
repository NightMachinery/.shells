#!/usr/bin/env -S sbcl_batteriful --script
;; Deprecated compatibility entry point. Rendering lives in the Go helper.
(format *error-output* "semantic_scholar_json_to_org.lisp is deprecated; use org-paper-metadata --from-json.~%")
(multiple-value-bind (output errors code)
    (uiop:run-program
     (list "zsh" "-c"
           "h-org-paper-tools-dep || exit $?; command org-paper-metadata --from-json --format org \"$1\""
           "_" (or (car (argv-get)) ""))
     :input *standard-input* :output *standard-output*
     :error-output *error-output* :ignore-error-status t)
  (declare (ignore output errors))
  (sb-ext:exit :code (or code 1)))
