(ns advent2020.day3
  (:require [grid :as grid]
            [utils :as utils]))


(defn answer-part1 [filename]
  (let [[dx dy] [3 1]
        grid (grid/parse (utils/read-input filename))
        [max-x max-y] (grid/bounds grid)]
    (loop [x 0 y 0 num-trees 0]
      (if (> y max-y)
        num-trees
        (recur
         (mod (+ x dx) max-x)
         (+ y dy)
         (if (= (get-in grid [y x]) \#)
           (inc num-trees)
           num-trees))))))

(answer-part1 "2020/day3-example.txt")
