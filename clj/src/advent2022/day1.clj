(ns advent2022.day1
  (:require [utils :as utils]))

(def lines (utils/read-resource-lines "input/day1.txt"))

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
