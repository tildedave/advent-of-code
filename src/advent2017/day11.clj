(ns advent2017.day11
  (:require [utils :as utils]))

(defn step [[x y] dir]
  (case dir
    "ne" [(inc x) (dec y)]
    "nw" [(dec x) (dec y)]
    "se" [(inc x) (inc y)]
    "sw" [(dec x) (inc y)]
    "n" [x (- y 2)]
    "s" [x (+ y 2)]))

(defn steps-away [x y]
  (quot (->> [x y]
       (map abs)
       (reduce +)) 2))

(defn num-steps [^String dirs]
   (->> (.split dirs ",")
        (reduce step [0 0])
        (apply steps-away)))

(num-steps "se,sw,se,sw,sw")
(num-steps "ne,ne,s,s")
(num-steps "ne,ne,ne")

(num-steps (first (utils/read-input "2017/day11.txt")))

(reductions
 step
 [0 0]
 (.split (first (utils/read-input "2017/day11.txt")) ","))

(defn max-num-steps [^String dirs]
  (->> (.split dirs ",")
       (reductions step [0 0])
       (map #(apply steps-away %))
       (reduce max)))

(max-num-steps (first (utils/read-input "2017/day11.txt")))
