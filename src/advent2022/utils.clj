(ns advent2022.utils
  (:require [clojure.java.io :as io]))

(defn read-resource-lines [resource]
  (line-seq (io/reader (io/resource resource))))

(defn parse-int [str] (Integer/valueOf str))

;; https://github.com/clojure/core.incubator/blob/be509fd967df8ce1ee43c43bca52360cf710252a/src/main/clojure/clojure/core/incubator.clj#L63-L75
(defn dissoc-in
  " Dissociates an entry from a nested associative structure returning a new
nested structure. keys is a sequence of keys. Any empty maps that result
will not be present in the new structure. "
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))
