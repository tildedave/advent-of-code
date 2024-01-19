(ns advent2022.day6
    (:require [advent2022.utils :as utils]))

(def lines (utils/read-resource-lines "input/day6.txt"))

(defn marker-start [n line]
  (loop [line line
         idx 0]
    (let [next (take n line)]
      (if (apply distinct? next) (+ n idx)
          (recur (rest line) (inc idx))))))

;; first star
(map #(marker-start 4 (seq %)) lines)

;; second star
(map #(marker-start 14 (seq %)) lines)
