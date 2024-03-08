(ns advent2021.day5
    (:require [utils :as utils]))

(def line-re #"(\d+),(\d+) -> (\d+),(\d+)")

(defn parse-line [line]
  (->> line
       (re-matches line-re)
       (rest)
       (map utils/parse-int)
       (partition 2)))

(defn vertical-or-horizontal? [[[x y] [x' y']]]
  (or (= x x') (= y y')))

(defn coords-in-line [[[x y] [x' y']]]
  (let [[dx dy] [(compare x x') (compare y y')]]
    (loop [x_ x'
           y_ y'
           result '()]
      (if
        (and (= x_ x) (= y_ y)) (conj result [x_ y_])
        (recur (+ x_ dx) (+ y_ dy) (conj result [x_ y_]))))))

(coords-in-line (parse-line "1,1 -> 3,3"))

(defn answer-part1 [lines]
  (->> lines
       (map parse-line)
       (filter vertical-or-horizontal?)
       (mapcat coords-in-line)
       (reduce (fn [m coord] (update m coord (fnil inc 0))) {})
       (filter #(> (second %) 1))
       (count)))

(answer-part1 (utils/read-resource-lines "input/day5-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day5.txt"))

(defn answer-part2 [lines]
  (->> lines
      (map parse-line)
      (mapcat coords-in-line)
      (reduce (fn [m coord] (update m coord (fnil inc 0))) {})
      (filter #(> (second %) 1))
      (count)))

(answer-part2 (utils/read-resource-lines "input/day5-example.txt"))
(answer-part2 (utils/read-resource-lines "input/day5.txt"))
