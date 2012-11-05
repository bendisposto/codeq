(ns datomic.codeq.analyzers.sicstus-pl
  (:require [datomic.api :as d]
            [datomic.codeq.util :refer [cond-> index->id-fn tempid?]]
            [datomic.codeq.analyzer :as az]   
            [clojure.java.io :as io])
  (:import java.io.File)
  (:gen-class))

(declare schemas analyze)

(defn ^java.io.Reader exec-stream 
  [^String cmd]
  (-> (Runtime/getRuntime)
      (.exec cmd) 
      .getInputStream 
      io/reader))

(deftype SicstusAnalyzer []
	  az/Analyzer
			  (keyname [a] :sicstus)
			  (revision [a] 2)
			  (extensions [a] [".pl"])
			  (schemas [a] (schemas))
			  (analyze [a db f src] (analyze db f src)))
			
			
(defn schemas [] {})

(defn- read-commit [src] 
    (let [file (.getAbsolutePath (File/createTempFile "codeq" ".pl")) 
        filename (str "../analyzer.sh " file)]
   
      (spit file src)
      (with-open [s (exec-stream filename)]
        (read-string (slurp s)))))

(defn analyze [db f src] 
  (let [info (read-commit src)]

    (println (update-in info [:victory] (constantly "brilliant")))))

(defn impl [] (SicstusAnalyzer.))