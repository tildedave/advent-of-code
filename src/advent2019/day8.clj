(ns advent2019.day8
  (:require [utils :as utils]))

(defn layers [w h seq]
  (partition h (partition w seq)))

(defn num-ones [layer]
  (count (filter zero? (flatten layer))))

(layers 3 2 '(1 2 3 4 5 6 7 8 9 0 1 2))

(defn answer-part1 [layer]
  (let [res (group-by identity (flatten layer))]
    (* (count (res 1)) (count (res 2)))))

;; part 1 answer
(->> (utils/read-input "2019/day8.txt")
     (first)
     (seq)
     (map #(utils/parse-int (str %)))
     (layers 25 6)
     (sort-by num-ones)
     (first)
     (answer-part1))
