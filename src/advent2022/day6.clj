(ns advent2022.day6
    (:require [advent2022.utils :as utils]))

(def lines (utils/read-resource-lines "input/day6.txt"))

(defn badge-start [line]
  (loop [line line
         idx 0]
    (let [next (take 4 line)]
      (if (apply distinct? next) (+ 4 idx)
          (recur (rest line) (inc idx))))))

