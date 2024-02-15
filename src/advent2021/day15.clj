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
        (> nodes 1000000) (throw (Exception. "too many nodes"))
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

(answer-part1 (utils/read-input "day15.txt"))
(answer-part1 (utils/read-input "day15-example.txt"))

;; part2 is just part1 with slightly different grid logic.
;; I think we can expand our neighbors, heuristic, and distance functions.

(defn out-of-bounds-part2? [grid [x y]]
  (let [[xmax ymax] (grid/bounds grid)]
    (or (< x 0)
        (< y 0)
        (>= x (* 5 xmax))
        (>= y (* 5 ymax)))))

(defn neighbors-part2 [grid [x y]]
  (remove
   nil?
   (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]]
     (let [[nx ny] [(+ x dx) (+ y dy)]]
       (if (out-of-bounds-part2? grid [nx ny])
         nil
         [nx ny])))))

(defn heuristic-part2 [grid [x y]]
  (let [[xmax ymax] (grid/bounds grid)]
    (+ (- (* 5 xmax) x) (- (* 5 ymax) y))))

(defn distance-part2 [grid _ [x y]]
  ;; manhattan distance from tile number of x y to
  ;; original tile, wrapping around.
  (let [[xmax ymax] (grid/bounds grid)
        [ox oy] [(mod x xmax) (mod y ymax)]
        num-tiles (+ (quot y ymax) (quot x xmax))
        new-risk (+ (get-in grid [oy ox]) num-tiles)]
    (if (> new-risk 9) (- new-risk 9) new-risk)))


(neighbors-part2
 (grid/parse (utils/read-input "day15-example.txt") (fn [ch] (Integer/valueOf (str ch))))
;;  "abc"
 [9 9])

(defn answer-part2 [lines]
  (let [grid (grid/parse lines (fn [ch] (Integer/valueOf (str ch))))
        goal (mapv (fn [n] (dec (* n 5))) (grid/bounds grid))]
    (a*-search [0 0] goal
               (partial neighbors-part2 grid)
               (partial heuristic-part2 grid)
               (partial distance-part2 grid))))

(answer-part2 (utils/read-input "day15-example.txt"))
(answer-part2 (utils/read-input "day15.txt"))
