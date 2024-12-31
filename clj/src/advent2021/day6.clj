(ns advent2021.day6
  (:require [utils :as utils]))

(defn parse-fish [num-list]
  (reduce
   (fn [m n] (update m n (fnil inc 0)))
   {}
   num-list))

(use 'clojure.tools.trace)
;; "fish" is now a map of number of fish.
(defn run-fish [fish]
  (apply
   merge-with +
   (mapcat
    (fn [[x n]] (if (= x 0) [{6 n} {8 n}] [{(dec x) n}]))
    fish)))

(defn lines-to-fish-seq [lines]
  (->> (.split (first lines) ",")
       (map utils/parse-int)
       (parse-fish)
       (iterate run-fish)))

(defn answer [t lines]
  (->>
   (->
    (lines-to-fish-seq lines)
    (nth t)
    (vals))
   (reduce +)))

(defn answer-part1 [lines] (answer 80 lines))
(defn answer-part2 [lines] (answer 256 lines))

(answer-part1  (utils/read-resource-lines "input/day6-example.txt"))
(answer-part1  (utils/read-resource-lines "input/day6.txt"))

(answer-part2  (utils/read-resource-lines "input/day6-example.txt"))
(answer-part2  (utils/read-resource-lines "input/day6.txt"))
