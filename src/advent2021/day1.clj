(ns advent2021.day1
  (:require [utils :as utils]))

(def example-lines (utils/read-resource-lines "input/day1-example.txt"))
(def input-lines (utils/read-resource-lines "input/day1.txt"))

(defn answer-part1 [lines]
  (let [parsed-lines (map utils/parse-int lines)]
  (->> (map vector parsed-lines (rest parsed-lines))
       (filter #(> (second %) (first %)))
       (count))))

(answer-part1 example-lines)
(answer-part1 input-lines)

(defn answer-part2 [lines]
  (let [parsed-lines (map utils/parse-int lines)
        merged-lines  (loop [lines parsed-lines
                            result []]
                       (if (< (count lines) 3) result
                           (recur
                            (rest lines)
                            (conj result (reduce + (take 3 lines))))))]
    (->>
     (map vector merged-lines (rest merged-lines))
     (filter #(> (second %) (first %)))
     (count))))

(answer-part2 input-lines)
