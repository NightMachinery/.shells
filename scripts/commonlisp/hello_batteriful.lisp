#!/usr/bin/env -S sbcl_batteriful --script

(org-quote-write :content (format nil "~a (~a)" "The northstar lives in the past yet ..." (alexandria:mean (list 1 2 3))))
