(ns advent2022.day1
  (:require [clojure.java.io :as io]))

(def lines
  (line-seq (io/reader (io/resource "input/day1.txt"))))

(def totals
  (->> lines
     (partition-by (fn [str] (= "" str)))
     (filter #(not= % (list "")))
     (map (partial map (fn [s] (Integer/valueOf s))))
     (map (partial reduce +))))

;; star 1 answer
(reduce max totals)

;; star 2 answer
(reduce + (take 3 (reverse (sort totals))))
