;; (ns NightMachinary.org-log-date-get
;;       (:require [org-parser.core :refer [read-str write-str]])
;;       ;; (:gen-class)
;;       )
;;;
(def kernel-gateway-p
  (=
   (get (System/getenv) "KERNEL_GATEWAY")
   "1"))

(defn defined? [sym]
  (let [var (resolve sym)]
    (and var (bound? var))))
;;;
(def needs-reload (not (defined? 'org-log-date-get)))

;; @warn we need to do 'require's in separate top-level forms from their usage
;;   https://stackoverflow.com/questions/26366251/how-to-require-namespace-inside-function-main
(when kernel-gateway-p
  (require '[clojupyter.misc.helper]))

(when needs-reload
  (when kernel-gateway-p
    ((resolve  'clojupyter.misc.helper/add-dependencies) '[org-parser "0.1.4"]))

  (require '[org-parser.core]))

(when needs-reload
  (defn org-log-date-get
    [& files]
    (doseq [file files]
      (let [input (slurp file)
            ast (org-parser.core/read-str input)]
        (def g_ast ast)
        ;; (identity g_ast)

        (println
         (loop [ast g_ast]
           (let [e (first ast)]
             (if (= (first e) :headline)
               (let [title (get (first (rest e)) :title nil)]
                 (if (and title (not (= title "")))
                   title
                   (recur (rest ast))))
               (recur (rest ast)))))))))

  (defn -main
    [& args]
    (apply org-log-date-get args)))

