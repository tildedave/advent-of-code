(ns advent2022.day4
  (:require [advent2022.utils :as utils]))

(def lines (utils/read-resource-lines "input/day4-example.txt"))

(defn to-range [s]
  (map utils/parse-int (.split s "-")))

(defn range-contained? [r1 r2]
  (and
   (>= (first r1) (first r2))
      (<= (second r1) (second r2))))

;; answer to part 1
(->> lines
     (map (fn [s] (.split s ",")))
     (map (partial map to-range))
     (map (fn [l] (or
                   (range-contained? (first l) (second l))
                   (range-contained? (second l) (first l)))))
     (map (fn [b] (if b 1 0)))
     (reduce +))


;; quite similar to an AOC 2019 puzzle.
(defn range-overlap? [r1 r2]
  (not (or (and (< (first r1) (first r2))
                (< (second r1) (first r2)))
           (and (> (first r1) (second r2)
                (> (second r1) (second r2)))))))

(range-overlap? [2 4] [6 8])

;; answer to part 2
(->> lines
     (map (fn [s] (.split s ",")))
     (map (partial map to-range))
     (map (fn [l] (or
                   (range-overlap? (first l) (second l))
                   (range-overlap? (second l) (first l)))))
     (map (fn [b] (if b 1 0)))
     (reduce +))
