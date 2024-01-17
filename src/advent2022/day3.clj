(ns advent2022.day3
  (:require [advent2022.utils :as utils]
            [clojure.set :as set]))

(def lines (utils/read-resource-lines "input/day3-example.txt"))

(defn split-at-halfway [line]
  (let [idx (/ (.length line) 2)]
    [(.subSequence line 0 idx), (.substring line idx)]))

(defn priority [ch]
  (let [code (int ch)]
    (cond (and (>= code 65) (<= code 90)) (- code 38)
          (and (>= code 97) (<= code 122)) (- code 96))))

;; answer to part 1
(->> lines
     (map (fn [line]
            (let [[f s] (split-at-halfway line)]
              (set/intersection (set f) (set s)))))
     (map (fn [set] (reduce + (map priority set))))
     (reduce +))

(defn get-badge [line-group]
  (priority (first (reduce set/intersection (map set line-group)))))

;; answer to part 2
(reduce + (map get-badge (partition 3 lines)))
