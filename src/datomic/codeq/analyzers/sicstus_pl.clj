(ns datomic.codeq.analyzers.sicstus-pl
  (:require [datomic.api :as d]
            [datomic.codeq.util :refer [cond-> index->id-fn tempid?]]
            [datomic.codeq.analyzer :as az]   
            [clojure.java.io :as io])
  (:import java.io.File))

(declare schemas analyze)

(defn ^java.io.Reader exec-stream 
  [^String cmd]
  (-> (Runtime/getRuntime)
      (.exec cmd) 
      .getInputStream 
      io/reader))

(defn sha->id [db sha] ((index->id-fn db :code/sha) sha))
(defn codename->id [db n] ((index->id-fn db :code/name) n))

(deftype SicstusAnalyzer []
  az/Analyzer
  (keyname [a] :sicstus)
  (revision [a] 2)
  (extensions [a] [".pl"])
  (schemas [a] (schemas))
  (analyze [a db f src] (analyze db f src)))

(defn- read-commit [src] 
  (let [file     (.getAbsolutePath (File/createTempFile "codeq" ".pl")) 
        filename (str "../analyzer.sh " file)]
    (spit file src)
    (with-open [s (exec-stream filename)]
      (read-string (slurp s)))))

(defn mk-export [db [module predicate arity]] 
  (let [pred-sha (az/sha (str module ":" predicate "/" arity))
        name-id (codename->id db predicate)
        pred-id (sha->id db pred-sha)
        module-id (codename->id db module)]
    [ {:db/id name-id :code/name predicate}
      {:db/id pred-id :prolog/predicatename name-id  :predicate/arity arity :predicate/module module-id}]))

(defn create-module [{ :keys [module exports import_module import_predicates] :as info} db src datoms]
  (let [ name-id (codename->id db module)
        name-tx (if (tempid? name-id) [{:db/id name-id :code/name module}] [])
        exports-tx (mapcat (partial mk-export db) exports)
                                        ; add module enity
                                        ; add exports
                                        ; add used modules
                                        ; add used predicates
        
        ]
    
    (println name-tx exports-tx)
    
    ))

(defn create-predicates  [_ _ _ _])

(defn analyze [db f src] 
  (let [info (read-commit src)]
    (->> [] (create-module info db src) (create-predicates info db src))))

(defn impl [] (SicstusAnalyzer.))

(defn schemas []
  {1 [{:db/id #db/id[:db.part/db]
       :db/ident :prolog/module
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/one
       :db/doc "prolog module name"
       :db.install/_attribute :db.part/db}
      
      {:db/id #db/id[:db.part/db]
       :db/ident :prolog/use_module
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/many
       :db/doc "prolog used modules"
       :db.install/_attribute :db.part/db}  
      
      {:db/id #db/id[:db.part/db]
       :db/ident :prolog/use_predicate
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/many
       :db/doc "prolog used predicates"
       :db.install/_attribute :db.part/db}  
      
      {:db/id #db/id[:db.part/db]
       :db/ident :prolog/export
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/many
       :db/doc "exported predicates"
       :db.install/_attribute :db.part/db}  
      
      {:db/id #db/id[:db.part/db]
       :db/ident :prolog/predicate
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/many
       :db/doc "A reference to a predicate module:predicate/arity"
       :db.install/_attribute :db.part/db}  
      
      {:db/id #db/id[:db.part/db]
       :db/ident :prolog/predicatename
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/one
       :db/doc "predicate name"
       :db.install/_attribute :db.part/db}  
      
      {:db/id #db/id[:db.part/db]
       :db/ident :predicate/module
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/one
       :db/doc "module that defines the predicate"
       :db.install/_attribute :db.part/db} 
      
      {:db/id #db/id[:db.part/db]
       :db/ident :predicate/arity
       :db/valueType :db.type/long
       :db/cardinality :db.cardinality/one
       :db/doc "Arity of the predicate"
       :db.install/_attribute :db.part/db}  
      
      {:db/id #db/id[:db.part/db]
       :db/ident :predicate/meta
       :db/valueType :db.type/boolean
       :db/cardinality :db.cardinality/one
       :db/doc "Predicate is a meta predicate"
       :db.install/_attribute :db.part/db}  
      
      {:db/id #db/id[:db.part/db]
       :db/ident :predicate/dynamic
       :db/valueType :db.type/boolean
       :db/cardinality :db.cardinality/one
       :db/doc "Predicate is a dynamic predicate"
       :db.install/_attribute :db.part/db}  
      
      {:db/id #db/id[:db.part/db]
       :db/ident :predicate/startline
       :db/valueType :db.type/long
       :db/cardinality :db.cardinality/one
       :db/doc "first line of the predicate"
       :db.install/_attribute :db.part/db}  
      
      {:db/id #db/id[:db.part/db]
       :db/ident :predicate/endline
       :db/valueType :db.type/long
       :db/cardinality :db.cardinality/one
       :db/doc "last line of the predicate"
       :db.install/_attribute :db.part/db}  
      ]})
