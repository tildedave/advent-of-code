(ns advent2021.day15
  (:require [advent2021.utils :as utils])
  (:require [advent2021.grid :as grid]))

;; we will try A*, again.
(grid/parse
 (utils/read-input "day15-example.txt")
 (fn [ch] (Integer/valueOf (str ch))))

(defn heuristic [grid [x y]]
  (let [[xmax ymax] (grid/bounds grid)]
    (+ (- xmax x) (- ymax y))))

(defn distance [grid [x y] [nx ny]]
  (get-in grid [ny nx]))

(defn answer-part1 [lines]
  (let [grid (grid/parse lines (fn [ch] (Integer/valueOf (str ch))))
        goal (mapv dec (grid/bounds grid))]
    (grid/a*-search [0 0] goal
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

(defn answer-part2 [lines]
  (let [grid (grid/parse lines (fn [ch] (Integer/valueOf (str ch))))
        goal (mapv (fn [n] (dec (* n 5))) (grid/bounds grid))]
    (grid/a*-search [0 0] goal
               (partial neighbors-part2 grid)
               (partial heuristic-part2 grid)
               (partial distance-part2 grid))))

(answer-part2 (utils/read-input "day15-example.txt"))
(answer-part2 (utils/read-input "day15.txt"))
