(ns advent2021.day15
  (:require [advent2021.utils :as utils])
  (:require [advent2021.grid :as grid])
  (:require [clojure.data.priority-map :refer [priority-map]]))

;; we will try A*, again.
(grid/parse
 (utils/read-input "day15-example.txt")
 (fn [ch] (Integer/valueOf (str ch))))

(defn heuristic [grid [x y]]
  (let [[xmax ymax] (grid/bounds grid)]
    (+ (- xmax x) (- ymax y))))

(defn distance [grid [x y] [nx ny]]
  (get-in grid [ny nx]))

;; we search for the end.
;; cost to get to the end is just our risk.
;; I am going to extract this to a common function because I'm tired of
;; programming A* search :-)
(defn a*-search [start goal neighbors heuristic distance]
  (loop [[goal-score open-set nodes]
         [{start 0}
         (priority-map start 0)
          0]]
    (if-let [[current _] (peek open-set)]
      (cond
        (= current goal) (goal-score current)
        (> nodes 100000) (throw (Exception. "too many nodes"))
        :else (recur
        (reduce
         (fn [[goal-score open-set nodes] neighbor]
           (let [n-distance (distance current neighbor)
                 n-new-score (+ (goal-score current) n-distance)
                 n-score (get goal-score neighbor Integer/MAX_VALUE)]
             (if (< n-new-score n-score)
               [(assoc goal-score neighbor n-new-score)
               (assoc open-set neighbor (+ n-new-score (heuristic neighbor)))
                nodes]
               [goal-score open-set nodes])))
         [goal-score (pop open-set) (inc nodes)]
         (neighbors current))))
      (throw (Exception. "could not reach goal")))))

(defn answer-part1 [lines]
  (let [grid (grid/parse lines (fn [ch] (Integer/valueOf (str ch))))
        goal (mapv dec (grid/bounds grid))]
    (a*-search [0 0] goal
               (partial grid/neighbors grid)
               (partial heuristic grid)
               (partial distance grid))))

(println
 (answer-part1 (utils/read-input "day15.txt")))
