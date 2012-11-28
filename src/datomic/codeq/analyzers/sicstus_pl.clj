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

(defn create-predicates  [_ _ _ _])




(defn mk-path [s] (apply str (cons "." (interleave (repeat "/") (rest (.split s "/"))))))

(defn git-checkout [commit]
  (when-not (= commit @current-commit)
    (print "Checking out " commit " ... ")
    (let [s (sh "git" "checkout" commit)]
      (if-not (= 0 (:exit s)) (println "fail.\n*** " (:err s))
           (do (println "ok.")  (reset! current-commit commit))))))

(defn process-used-module [db codeq used]
  (let [id (from-name db (.trim used))
        codeq-id (or (ffirst (d/q '[:find ?e :in $ ?id :where [?e :prolog/type :type/module] [?e :module/name ?id]] db id))
                    (d/tempid :db.part/user))]
    (cond-> [[:db/add codeq :module/use_module codeq-id]]
            (tempid? id) (conj {:db/id id :code/name used})
            (tempid? codeq-id) (conj {:db/id codeq-id
                                      :prolog/type :type/module
                                      :module/name id }))))

(defn process-used-predicates [db codeq [module predicate arity]]
  (let [predicate-id (from-name db predicate)
        module-id (from-name db module)
        new-predicate-id? (tempid? predicate-id)
        new-module-id? (tempid? module-id)
        codeq-id (or (ffirst (d/q '[:find ?e :in $ ?m ?p ?a
                                    :where [?e :prolog/type :type/predicate]
                                    [?e :predicate/name ?p]
                                    [?e :predicate/arity ?a]
                                    [?e :predicate/module ?m]]
                                  db module-id predicate-id arity))
                     (d/tempid :db.part/user))]
    (cond-> []
            new-predicate-id? (conj {:db/id predicate-id :code/name predicate})
            new-module-id? (conj {:db/id module-id :code/name module})
            (tempid? codeq-id)  (conj {:db/id codeq-id
                                       :prolog/type :type/predicate
                                       :predicate/module module-id
                                       :predicate/name predicate-id
                                       :predicate/arity arity})
            :always (conj [:db/add codeq :module/use_predicate codeq-id]))))

(defn process-info [db {:keys [module module_startline module_endline import_module import_predicates] :as info} src filename f]
  (let [lc (last-column src module_endline)
        name-id (from-name db module)
        code-id (from-code db module)
        new-name-id? (tempid? name-id)
        new-code-id? (tempid? code-id)
        loc (str module_startline " " 1 " " module_endline " " lc)
        codeq-id (or (ffirst (d/q '[:find ?e :in $ ?id
                                    :where [?e :module/name ?id] [?e :prolog/type :type/module]]
                                  db name-id))
                     (d/tempid :db.part/user))
        new-codeq-id? (tempid? codeq-id)]
    
    (cond-> []
            new-code-id? (conj {:db/id code-id :code/sha (az/sha module) :code/text module})
            new-name-id? (conj {:db/id name-id :code/name module})
            new-codeq-id? (conj {:db/id codeq-id})
            :always (conj [:db/add codeq-id :codeq/file f]
                          [:db/add codeq-id :codeq/loc loc]
                          [:db/add codeq-id :prolog/type :type/module]
                          [:db/add codeq-id :codeq/code code-id]
                          [:db/add codeq-id :module/name name-id])
            :always (concat (mapcat (partial process-used-module db codeq-id) import_module))
            :always (concat (mapcat (partial process-used-predicates db codeq-id) import_predicates))
            )))

(defn mk-sicstus-call [filename]
  (str "prolog_flag(redefine_warnings, _, off),on_exception(X,(use_module('"
       filename
       "'),write_clj_representation,halt),(print('{:error \"'),print(X),print('\"}'),nl,halt(1)))."))

(defn analyze-file [db filename src f]
  (let [t (sh "sicstus" "-l" "../codeq_analyzer.pl" "--goal"
              (mk-sicstus-call filename))
         output (:out t)
        info (read-string output)]
    (assert (< 0 (count info)) "** Error: Prolog returned empty result")
    (when-not (= 0 (:exit t)) (println "*** Error: " (:err t)))
    (if (:error info)
      (println "*** Error:" (:error info))
      (process-info db info src filename f))))

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
  {1 [[:db/add #db/id[:db.part/user] :db/ident :type/module]
      [:db/add #db/id[:db.part/user] :db/ident :type/predicate]
      [:db/add #db/id[:db.part/user] :db/ident :type/clause]
      
      (attr prolog/type one ref "A prolog codeq is either a module, a predicate or a clause")
      (attr module/name one ref "The name of the module. Reference to codename entity.")
      (attr module/use_module many ref "Modules that are used by this module. References to other module's codeq entitie.s")
      (attr module/use_predicate many ref "Imported predicates. Reference to predicate's codeqs.")
      (attr module/export many ref "Exported predicates. Reference to predicate's codeqs.")

      (attr predicate/module one ref "The name of the containing module")
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
