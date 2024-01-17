(ns advent2022.day1
  (:require [clojure.java.io :as io]))

(defn read-input-file [filename]
  (with-open [rdr (io/reader filename)]
    (doseq [line (line-seq rdr)]
      (print line))))

(def lines
  (line-seq (io/reader (io/resource "input/day1.txt"))))


(def parsed-lines-str
  (filter #(not= % (list ""))
          (partition-by (fn [str] (= "" str)) lines)))

(def parsed-lines
  (map (partial map (fn [s] (Integer/valueOf s))) parsed-lines-str))

(def totals (map (partial reduce +) parsed-lines))

;; star 1 answer
(reduce max totals)

;; star 2 answer
(reduce + (take 3 (reverse (sort totals))))
