(ns datomic.codeq.analyzers.sicstus-pl
  (:require [datomic.api :as d]
            [datomic.codeq.util :refer [cond-> index->id-fn tempid?]]
            [datomic.codeq.analyzer :as az]   
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]])
  (:import java.io.File))

(declare schemas analyze get-commit)

(deftype SicstusAnalyzer []
  az/Analyzer
  (keyname [a] :sicstus)
  (revision [a] 2)
  (extensions [a] [".pl"])
  (schemas [a] (schemas))
  (analyze [a db f src] (analyze db f src)))

(defn mk-export [{:keys [codename->id sha->id]} [module predicate arity]] 
  (let [pred-sha (az/sha (str module ":" predicate "/" arity))
        name-id (codename->id predicate)
        pred-id (sha->id pred-sha)
        module-id (codename->id module)]
    [ {:db/id name-id :code/name predicate}
      {:db/id pred-id :prolog/predicatename name-id  :predicate/arity arity :predicate/module module-id}]))

;; (defn mk-module-import [ctx mod] (let [module-d (codename->id module
;;                                                  )]   )  )


(defn create-module [{ :keys [module exports import_module import_predicates] :as info} {:keys [codename->id sha->id added] :as ctx} src datoms]
  ;; (let [name-id (codename->id module)
  ;;       newid? (and (tempid? name-id) (not (added name-id)))
  ;;       name-tx (if newid? [{:db/id name-id :code/name module}] [])
  ;;       added (if newid? (conj added name-id) added) 
  ;;       exports-tx  (mapcat (partial mk-export ctx) exports)
  ;;       import-mod-tx []; (mapcat (partial mk-module-import ctx) import_module)
  ;;                                       ; add used modules
  ;;                                       ; add used predicates
  ;;       transaction (concat name-tx exports-tx)]

  ;; ;  (println name-tx exports-tx import-mod-tx)
  ;;   transaction)
  []
  )

(defn create-predicates  [_ _ _ _])

(defn mk-path [s] (apply str (cons "." (interleave (repeat "/") (rest (.split s "/"))))))


(defn process-commit [db src [commit file]]
  (let [filename (mk-path file)
        ctx { :sha->id (index->id-fn db :code/sha)
             :codename->id (index->id-fn db :code/name)
             :added #{}
             }]
    (println "Checking out commit " commit " to analyze " filename)
    (let [s (sh "git" "checkout" commit)
          t (sh "../analyzer.sh" filename)
          output (:out t)
          info (read-string output)]
      (assert (< 0 (count info)))
      (if (seq (:err s)) (println "*** " (:err s))) 
      (if (not= 0 (:exit t)) (println "*** " (:err t)))
      (if (:error info)
        (println "*** Error:" (:error info)) 
        (create-module info ctx src [])))))

(defn analyze [db f src] 
  (let [sha (:git/sha (d/entity db f))
        commit-info (get-commit db sha)]
     (mapcat (partial process-commit db src) commit-info)))

(defn impl [] (SicstusAnalyzer.))

(defmacro attr 
  ([name cardinality type] `(attr ~name ~cardinality ~type {}))
  ([name cardinality type other]
  `(merge ~other {:db/id ~(d/tempid :db.part/db)
    :db/ident ~(keyword (str name))
    :db/valueType ~(keyword "db.type" (str type))
    :db/cardinality ~(keyword "db.cardinality" (str cardinality))
    :db.install/_attribute :db.part/db})))

(defn schemas []
  {1 [(attr prolog/module one ref)
      (attr prolog/use_module many ref)
      (attr prolog/use_predicate many ref)
      (attr prolog/export many ref)
      (attr prolog/predicate many ref)
      (attr prolog/predicatename one ref)
      (attr predicate/module one ref)
      (attr predicate/arity one long)
      (attr predicate/dynamic one boolean)
      (attr predicate/meta one boolean)
      (attr predicate/dynamic one boolean)
      (attr predicate/startline one long)
      (attr predicate/endline one long)]})

(defn get-commit [db sha]
;  (print "Searching for " sha)
  (let [res (d/q
          '[:find ?commit-sha ?file-name
            :in $ ?sha
            :where
            [?file-id :git/sha ?sha ?tx]
            [?commit-id :git/type :commit ?tx]
            [?commit-id :git/sha ?commit-sha]
            [?obj :node/object ?file-id]
            [?obj :node/paths ?fne]
            [?fne :file/name ?file-name]] db sha)]
 ;   (println " yields commit " res)
    res))

