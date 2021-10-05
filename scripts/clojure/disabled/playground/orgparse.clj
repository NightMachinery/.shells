;; #!/usr/bin/env -S clj -Sdeps 'org-parser/org-parser {:mvn/version "0.1.4"}' --main
(require '[clojupyter.misc.helper :as helper])
(helper/add-dependencies '[org-parser "0.1.4"])

(ns orgparse-playground
  (:require [org-parser.core :refer [read-str write-str]]))

(read-str "* Headline")
