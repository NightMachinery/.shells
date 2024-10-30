#!/usr/bin/env bb
;; @broken
;; @o1 , @Claude/3.5-sonnet-new
;;;

(require '[babashka.deps :as deps])

;; Add dependencies
(deps/add-deps
 '{:deps {org.clojure/data.json {:mvn/version "2.4.0"}}})

(require '[clojure.data.json :as json])
(require '[clojure.string :as str])

;; Function to escape titles for Org-mode links
(defn org-title-escape [s]
  (-> s
      (str/replace #"(?<!\\)\[" "{")
      (str/replace #"(?<!\\)\]" "}")))

;; Function to escape URLs for Org-mode links
(defn org-link-escape [s]
  (-> s
      (str/replace #"(?<!\\)\[" "\\\\[")
      (str/replace #"(?<!\\)\]" "\\\\]")))

;; Function to convert strings to CamelCase
(defn camel-case [s]
  (->> (str/split s #"[ -]")
       (map str/capitalize)
       (apply str)))

;; Mapping of venue name patterns to abbreviations
(def venue-name-mapping
  [[#"(?i)^arXiv(?:\.org)?$" "arXiv"]
   [#"(?i)IEEE/CVF International Conference on Computer Vision Workshop" "ICCVW"]
   [#"(?i)Conference on Computer Vision and Pattern Recognition Workshops" "CVPRW"]
   [#"(?i)European Conference on Computer Vision Workshops" "ECCVW"]
   [#"(?i)British Machine Vision Conference" "BMVC"]
   [#"(?i)Workshop on Action and Anticipation for Visual Learning" "AAVL"]
   [#"(?i)Workshop on Visual Object Tracking" "VOT"]
   [#"(?i)Workshop on the Applications of Computer Vision for Drone Technology" "ACVDT"]
   [#"(?i)Workshop on Large Scale 3D Data: Acquisition, Modelling and Analysis" "LS3DA"]
   [#"(?i)Workshop on Efficient Deep Learning for Computer Vision" "EDLCV"]
   [#"(?i)Workshop on Biometrics" "BIO"]
   [#"(?i)Workshop on Scene Understanding and Autonomous Systems" "SUAS"]
   [#"(?i)International Workshop on Artificial Intelligence for Cultural Heritage" "AI4CH"]
   [#"(?i)Workshop on Fairness, Accountability, and Transparency in Machine Learning" "FAT/ML"]
   [#"(?i)Workshop on Automated Knowledge Base Construction" "AKBC"]
   [#"(?i)Workshop on Machine Learning in Health" "ML4H"]
   [#"(?i)Workshop on Computer Vision for Augmented and Virtual Reality" "CV4ARVR"]
   [#"(?i)Workshop on Systems for ML" "SysML"]
   [#"(?i)Workshop on Robot Learning" "CoRL"]
   [#"(?i)Deep Learning for Real-Time Graphics Workshop" "DLRTG"]
   [#"(?i)Workshop on Machine Learning for Creativity and Design" "ML4AD"]
   [#"(?i)BlackboxNLP Workshop on Analyzing and Interpreting Neural Networks for NLP" "BlackboxNLPW"]
   [#"(?i)ACL Workshop" "ACLW"]
   [#"(?i)Annual Meeting of the Association for Computational Linguistics" "ACL"]
   [#"(?i)North American Chapter of the Association for Computational Linguistics" "NAACL"]
   [#"(?i)European Chapter of the Association for Computational Linguistics" "EACL"]
   [#"(?i)Asian Chapter of the Association for Computational Linguistics" "AACL"]
   [#"(?i)Transactions of the Association for Computational Linguistics" "TACL"]
   [#"(?i)Conference on Computational Natural Language Learning" "CoNLL"]
   [#"(?i)Computational Linguistics" "CL"]
   [#"(?i)Journal of Natural Language Engineering" "JNLE"]
   [#"(?i)(?:Conference on )?Computer Vision and Pattern Recognition" "CVPR"]
   [#"(?i)(?:Conference on )?Uncertainty in Artificial Intelligence" "UAI"]
   [#"(?i)(?:Conference on )?Empirical Methods in Natural Language Processing" "EMNLP"]
   [#"(?i)International Conference on Computer Vision" "ICCV"]
   [#"(?i)European Conference on Computer Vision" "ECCV"]
   [#"(?i)Neural Information Processing Systems" "NeurIPS"]
   [#"(?i)International Conference on Machine Learning" "ICML"]
   [#"(?i)AAAI Conference on Artificial Intelligence" "AAAI"]
   [#"(?i)International Joint Conference on Artificial Intelligence" "IJCAI"]
   [#"(?i)International Conference on Artificial Intelligence and Statistics" "AISTATS"]
   [#"(?i)International Conference on Learning Representations" "ICLR"]
   [#"(?i)IEEE International Conference on Robotics and Automation" "ICRA"]
   [#"(?i)IEEE/RSJ International Conference on Intelligent Robots and Systems" "IROS"]
   [#"(?i)Journal of Machine Learning Research" "JMLR"]
   [#"(?i)ACM SIGKDD International Conference on Knowledge Discovery and Data Mining" "KDD"]
   [#"(?i)ACM Conference on Information and Knowledge Management" "CIKM"]
   [#"(?i)ACM Symposium on Theory of Computing" "STOC"]
   [#"(?i)ACM SIGCHI Conference on Human Factors in Computing Systems" "CHI"]
   [#"(?i)ACM SIGGRAPH" "SIGGRAPH"]
   [#"(?i)ACM Transactions on Graphics" "TOG"]
   [#"(?i)ACM SIGCOMM" "SIGCOMM"]
   [#"(?i)ACM SIGMOD International Conference on Management of Data" "SIGMOD"]
   [#"(?i)ACM International Conference on Supercomputing" "ICS"]
   [#"(?i)ACM/IEEE Design Automation Conference" "DAC"]
   [#"(?i)ACM/IEEE International Symposium on Computer Architecture" "ISCA"]
   [#"(?i)IEEE/CVF Winter Conference on Applications of Computer Vision" "WACV"]
   [#"(?i)IEEE Transactions on Pattern Analysis and Machine Intelligence" "TPAMI"]
   [#"(?i)IEEE Transactions on Neural Networks and Learning Systems" "TNNLS"]
   [#"(?i)IEEE Transactions on Image Processing" "TIP"]
   [#"(?i)IEEE Transactions on Robotics" "T-RO"]
   [#"(?i)IEEE Transactions on Cybernetics" "TCYB"]
   [#"(?i)Pattern Recognition Letters" "PRL"]
   [#"(?i)Journal of Artificial Intelligence Research" "JAIR"]
   [#"(?i)Journal of Computer Vision and Image Understanding" "CVIU"]
   [#"(?i)Knowledge-Based Systems" "KBS"]
   [#"(?i)ACM Transactions on Intelligent Systems and Technology" "TIST"]
   [#"(?i)Artificial Intelligence Journal" "AIJ"]
   [#"(?i)ACM SIGSOFT International Symposium on Software Testing and Analysis" "ISSTA"]
   [#"(?i)International Symposium on Software Reliability Engineering" "ISSRE"]
   [#"(?i)USENIX Annual Technical Conference" "USENIX ATC"]
   [#"(?i)USENIX Symposium on Networked Systems Design and Implementation" "NSDI"]
   [#"(?i)USENIX Security Symposium" "USENIX Security"]
   [#"(?i)International Symposium on Computer Architecture" "ISCA"]
   [#"(?i)IEEE Symposium on Security and Privacy" "S&P"]
   [#"(?i)ACM Conference on Embedded Networked Sensor Systems" "SenSys"]
   [#"(?i)ACM International Conference on Multimedia" "ACM MM"]
   [#"(?i)ACM International Systems and Storage Conference" "SYSTOR"]
   [#"(?i)IEEE Transactions on Mobile Computing" "TMC"]
   [#"(?i)IEEE Transactions on Knowledge and Data Engineering" "TKDE"]
   [#"(?i)ACM Transactions on Information Systems" "TOIS"]
   [#"(?i)International Conference on Very Large Data Bases" "VLDB"]
   [#"(?i)ACM SIGSOFT Symposium on the Foundations of Software Engineering" "FSE"]
   [#"(?i)International Conference on Functional Programming" "ICFP"]
   [#"(?i)ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages" "POPL"]
   [#"(?i)ACM SIGPLAN Conference on Programming Language Design and Implementation" "PLDI"]])

;; Function to get the venue abbreviation
(defn venue-name-get [s]
  (or (some (fn [[pattern abbreviation]]
              (when (re-find pattern s)
                abbreviation))
            venue-name-mapping)
      (camel-case s)))

(defn determine-source [data]
  (cond
    (contains? data :feed) :arxiv
    (contains? data :abstract) :semantic-scholar
    :else (throw (ex-info "Unknown data source" {:data data}))))

(defn extract-entry [data source]
  (case source
    :arxiv (get-in data [:feed :entry])
    :semantic-scholar data))

(defn get-title [entry _]
  (str/trim (:title entry)))

(defn get-abstract [entry source]
  (str/trim
   (case source
     :arxiv (:summary entry)
     :semantic-scholar (:abstract entry))))

(defn get-authors [entry source]
  (case source
    :arxiv (let [authors (:author entry)]
             (map #(get % :name) (if (map? authors) [authors] authors)))
    :semantic-scholar (map #(get % :name) (:authors entry))))

(defn get-date [entry source]
  (case source
    :arxiv (:published entry)
    :semantic-scholar (:publicationDate entry)))

(defn get-url [entry source]
  (case source
    :arxiv (let [links (if (map? (:link entry)) [(:link entry)] (:link entry))
                pdf-link (some #(when (= (:title %) "pdf") (get % :@href %)) links)]
            (or pdf-link (:id entry)))
    :semantic-scholar (or (:url entry)
                        (first (:links entry)))))

(defn get-venue [entry source]
  (case source
    :arxiv "arXiv"
    :semantic-scholar (or (:venue entry) (:journal_name entry))))

(defn get-citation-count [entry source]
  (case source
    :arxiv nil
    :semantic-scholar (:citationCount entry)))

(def months
  {"01" "January"
   "02" "February"
   "03" "March"
   "04" "April"
   "05" "May"
   "06" "June"
   "07" "July"
   "08" "August"
   "09" "September"
   "10" "October"
   "11" "November"
   "12" "December"})

(defn extract-year [date-str]
  (when date-str
    (second (re-find #"(\d{4})" date-str))))

(defn extract-month [date-str]
  (when date-str
    (or (second (re-find #"([a-zA-Z]+)" date-str))
        (let [month-number (second (re-find #"\d{4}-(\d{2})" date-str))]
          (get months month-number)))))

(defn get-date-tag [date-str]
  (when-let [year (extract-year date-str)]
    (if-let [month (extract-month date-str)]
      (str "@" year "/" month)
      (str "@" year))))

(defn build-properties [entry source]
  (let [title (get-title entry source)
        url (get-url entry source)
        date (get-date entry source)
        authors (get-authors entry source)
        venue (get-venue entry source)
        citation-count (get-citation-count entry source)]
    (cond-> {}
      true (assoc :title title)
      true (assoc :url url)
      (seq authors) (assoc :authors (str/join ", " authors))
      venue (assoc :venue venue)
      date (assoc :date date)
      citation-count (assoc :citations citation-count)
      (= source :arxiv) (assoc :arxiv "yes"))))

(defn print-org-output [entry source]
  (let [properties (build-properties entry source)
        date-tag (some-> (get-date entry source) get-date-tag)
        title (get-title entry source)
        url (get-url entry source)
        abstract (get-abstract entry source)
        venue (get-venue entry source)
        venue-abbr (when venue (venue-name-get venue))
        citation-count (get-citation-count entry source)]
    ;; Output in Org-mode format
    (println (str (or date-tag "")
                  (when citation-count (str " @citations/" citation-count))
                  (when venue-abbr (str " @" venue-abbr))
                  " [["
                  (org-link-escape url)
                  "]["
                  (org-title-escape title)
                  "]]"))
    (println ":PROPERTIES:")
    (doseq [[k v] properties]
      (println (str ":" (name k) ": " v)))
    (println ":END:")
    (println "* @abstract")
    (println ":PROPERTIES:")
    (println ":visibility: folded")
    (println ":END:")
    (println "#+BEGIN_QUOTE")
    (println abstract)
    (println "#+END_QUOTE")))

;; Main execution
(let [data (json/read-str (slurp *in*) :key-fn keyword)
      source (determine-source data)
      entry (extract-entry data source)]
  (print-org-output entry source))
