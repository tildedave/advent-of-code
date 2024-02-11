(ns advent2021.day5
    (:require [advent2021.utils :as utils]))

(def line-re #"(\d+),(\d+) -> (\d+),(\d+)")

(defn parse-line [line]
  (->> line
       (re-matches line-re)
       (rest)
       (map utils/parse-int)
       (partition 2)))

(defn coords-in-line [[[x y] [x' y']]]
  (cond
    (= x x') (for [y (range (min y y') (inc (max y y')))]
               [x y])
    (= y y') (for [x (range (min x x') (inc (max x x')))]
               [x y])))


(coords-in-line (parse-line "9,7 -> 7,7"))

;; this is not right
(defn answer-part1 [lines]
  (->> lines
       (map parse-line)
       (mapcat coords-in-line)
       (reduce (fn [m coord] (update m coord (fnil inc 0))) {})
       (filter #(> (second %) 1))
       (count)))

(answer-part1 (utils/read-resource-lines "input/day5-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day5.txt"))
