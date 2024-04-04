(ns advent2015.day3
  (:require [utils :as utils]))

(defn visit [instructions]
  (reduce
   (fn [[houses-so-far [x y]] ch]
     (let [[nx ny] (case ch
                     \^ [x (dec y)]
                     \v [x (inc y)]
                     \> [(inc x) y]
                     \< [(dec x) y])]
       [(update houses-so-far [nx ny] (fnil inc 0))
        [nx ny]]))
   [{[0 0] 1} [0 0]]
   (seq instructions)))

(defn answer-part1 [instructions]
  (->> (visit instructions)
       (first)
       (keys)
       (count)))

(answer-part1 "^>v<")
(answer-part1 (first (utils/read-input "2015/day3.txt")))
