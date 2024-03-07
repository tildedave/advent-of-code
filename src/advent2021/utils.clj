(ns advent2021.utils
  (:require [clojure.java.io :as io]))

(defn read-resource-lines [resource]
  (line-seq (io/reader (io/resource resource))))

(defn read-input [str]
  (read-resource-lines (format "input/%s" str)))

(defn parse-int [str] (Integer/valueOf str))

(defn quot-round-up [n m]
  (case (mod n m)
    0 (quot n m)
    (inc (quot n m))))

(defn try-parse-int [str]
  (try
    (Integer/valueOf str)
    (catch IllegalArgumentException _ str)))


(defn sml-partition
  "SML's List.partition, which I find myself reaching for."
  ([f s] (sml-partition f s (list) (list)))
  ([f s true-list false-list]
   (loop [s s
          true-list true-list
          false-list false-list]
     (if-let [x (first s)]
       (case (f x)
             true (recur (rest s)
                         (conj true-list x)
                         false-list)
             false (recur (rest s)
                          true-list
                          (conj false-list x)))
       [true-list false-list]))))

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
