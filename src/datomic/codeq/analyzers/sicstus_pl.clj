(ns datomic.codeq.analyzers.sicstus-pl
  (:require [datomic.api :as d]
            [datomic.codeq.util :refer [cond-> index->id-fn tempid?]]
            [datomic.codeq.analyzer :as az]   
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]])
  (:import java.io.File))

(declare schemas analyze get-commit last-column)

(defn from-sha [db sha]  ((index->id-fn db :code/sha) sha))
(defn from-name [db name] ((index->id-fn db :code/name) name))
(defn from-code [db code] (from-sha db (az/sha code)))

(def current-commit (atom ""))


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

(defn create-codeq [file loc parent code]
  {:codeq/file file :codeq/loc loc :codeq/parent parent :codeq/code code})


(defn create-module [{ :keys [module exports import_module import_predicates] :as info} {:keys [codename->id sha->id added] :as ctx} src datoms]
  (let [name-id (codename->id module)
        newid? (and (tempid? name-id) (not (added name-id)))
        name-tx (if newid? [{:db/id name-id :code/name module}] [])
        added (if newid? (conj added name-id) added)
        exports-tx [] ; (mapcat (partial mk-export ctx) exports)
        import-mod-tx []; (mapcat (partial mk-module-import ctx) import_module)
                                        ; add used modules
                                        ; add used predicates
        transaction (concat name-tx exports-tx)]

    (println name-tx exports-tx import-mod-tx)
    transaction))

(defn create-predicates  [_ _ _ _])

(defn mk-path [s] (apply str (cons "." (interleave (repeat "/") (rest (.split s "/"))))))

(defn git-checkout [commit]
  (when-not (= commit @current-commit)
    (print "Checking out " commit " ... ")
    (let [s (sh "git" "checkout" commit)]
      (if-not (= 0 (:exit s)) (println "fail.\n*** " (:err s))
           (do (println "ok.")  (reset! current-commit commit))))))

(defn process-used-module [db codeq used]
  (let [id (from-name db (.trim used))]
    (cond-> [{:db/id codeq :module/use_module id}] (tempid? id) (conj {:db/id id :code/name used}))))

(defn process-info [db {:keys [module module_startline module_endline import_module] :as info} src filename f tx-data]
  (let [lc (last-column src module_endline)
        name-id (from-name db module)
        code-id (from-code db module)
        new-name-id? (tempid? name-id)
        new-code-id? (tempid? code-id)
        tx-data (cond-> tx-data new-code-id? (conj {:db/id code-id :code/sha (az/sha module) :code/text module}) )
        tx-data (cond-> tx-data new-name-id? (conj {:db/id name-id :code/name module}))
        loc (str module_startline " " 1 " " module_endline " " lc)
        codeq-id (or (ffirst (d/q '[:find ?e :in $ ?f ?loc
                                   :where [?e :codeq/file ?f] [?e :codeq/loc ?loc]]
                                 db f loc))
                    (d/tempid :db.part/user))
        new-codeq-id? (tempid? codeq-id)
        tx-data (cond-> tx-data new-codeq-id?
                        (conj {:db/id codeq-id
                               :codeq/file f
                               :codeq/loc loc
                               :codeq/code code-id
                               :module/name name-id }))
        tx-data (concat tx-data (mapcat (partial process-used-module db codeq-id) import_module))]
  
    (println :tx tx-data)
    tx-data
    ))

(defn analyze-file [db filename src f]
  (let [t (sh "../analyzer.sh" filename)
        output (:out t)
        info (read-string output)]
    (assert (< 0 (count info)) "** Error: Prolog returned empty result")
    (when-not (= 0 (:exit t)) (println "*** Error: " (:err t)))
    (if (:error info)
      (println "*** Error:" (:error info))
      (->> []
           (process-info db info src filename f)))))

(defn process-file [db src f [commit file]]
 
  (let [filename (mk-path file)]
    (git-checkout commit)
    (if (.exists (io/file filename))
      (do (println "Running analyzer on " filename " in commit " commit)
          (analyze-file db filename src f))
      (do  (println "*** Skipping nonexisting file " filename " in commit " commit)
           []))))

(defn analyze [db f src]
  (let [sha (:git/sha (d/entity db f))
        commit-info (get-commit db sha)]
     (mapcat (partial process-file db src f) commit-info)))

(defn impl [] (SicstusAnalyzer.))

(defmacro attr 
  ([name cardinality type doc]
     `(attr ~name ~cardinality ~type ~doc {}))
  ([name cardinality type doc other]
     `(merge ~other {:db/id ~(d/tempid :db.part/db)
                     :db/ident ~(keyword (str name))
                     :db/valueType ~(keyword "db.type" (str type))
                     :db/cardinality ~(keyword "db.cardinality" (str cardinality))
                     :db/doc ~doc
                     :db.install/_attribute :db.part/db})))

(defn schemas []
  {1 [(attr prolog/identity one string "The identity of the entity. For modules it's the name, for predicates its name/arity and for a clause it is the location.")

      (attr module/name one ref "The name of the module. Reference to codename entity.")
      (attr module/use_module many ref "Modules that are used by this module. References to other module's codeq entitie.s")
      (attr module/use_predicate many ref "Imported predicates. Reference to predicate's codeqs.")
      (attr module/export many ref "Exported predicates. Reference to predicate's codeqs.")
      
      (attr predicate/name one ref "The name of a predicate. Reference to codename entity.")
      (attr predicate/arity one long "Arity of the predicate. Long value.")
      (attr predicate/dynamic one boolean "True iff the predicate id declared dynamic.") 
      (attr predicate/meta one boolean "True iff the predicate id declared as a meta predicate.")

      (attr clause/calls many ref "Predicates called in a clause. Reference to predicate's codeqs.")
      (attr clause/startline one long "First line of the clause. Long value.")
      (attr clause/endline one long "Last line of the clause. Long value.")]})

(defn get-commit [db sha]
  (d/q '[:find ?commit-sha ?file-name
         :in $ ?sha
         :where
         [?file-id :git/sha ?sha ?tx]
         [?commit-id :git/type :commit ?tx]
         [?commit-id :git/sha ?commit-sha]
         [?obj :node/object ?file-id]
         [?obj :node/paths ?fne]
         [?fne :file/name ?file-name]] db sha))


(defn last-column [src line]
  (let [lines (vec (.split #"\r?\n" src))
        l (dec line)]
    (assert (<= 0 l (count lines)))
    (count (lines l))))
