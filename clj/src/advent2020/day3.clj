(ns advent2020.day3
  (:require [grid :as grid]
            [utils :as utils]))


(defn trees-for-slope [grid dx dy]
  (let [[max-x max-y] (grid/bounds grid)]
    (loop [x 0 y 0 num-trees 0]
      (if (> y max-y)
        num-trees
        (recur
         (mod (+ x dx) max-x)
         (+ y dy)
         (if (= (get-in grid [y x]) \#)
           (inc num-trees)
           num-trees))))))

(defn answer-part1 [filename]
  (let [grid (grid/parse (utils/read-input filename))]
    (trees-for-slope grid 3 1)))

(answer-part1 "2020/day3-example.txt")
(answer-part1 "2020/day3.txt")

(defn answer-part2 [filename]
  (let [grid (grid/parse (utils/read-input filename))]
    (->> [[1 1]
          [3 1]
          [5 1]
          [7 1]
          [1 2]]
         (map #(apply (partial trees-for-slope grid) %))
         (reduce *))))

(answer-part2 "2020/day3-example.txt")
(answer-part2 "2020/day3.txt")
