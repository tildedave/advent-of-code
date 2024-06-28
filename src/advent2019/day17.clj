(ns advent2019.day17
  (:require [clojure.core.async :as a :refer [<!! >! <! >!!]]
            [advent2019.intcode :as intcode]
            [clojure.string :as string]
            [grid :as grid]
            [clojure.core.match :refer [match]]))

(defn robot-grid []
  (let [output (intcode/run-file "2019/day17.txt")]
    (->> (<!! (a/into [] output))
         (partition-by (partial = 10))
         (take-nth 2)
         (map #(map (fn [n]
                      (case n
                        35 \#
                        46 \.
                        94 \^)) %)))))

(def example-grid
  '("..#.........."
   "..#.........."
   "#######...###"
   "#.#...#...#.#"
   "#############"
   "..#...#...#.."
   "..#####...^.."))

(grid/parse example-grid)


(defn scaffold-score [grid]
  (->> (grid/coords grid)
       (filter
        (fn [[x' y']]
          (and
           (= (grid/at grid [x' y']) \#)
           (every?
           (fn [[x y]] (= (grid/at grid [x y]) \#))
           (grid/neighbors grid [x' y'])))))
       (map #(apply * %))
       (reduce +)
       ))

;; part 1
(scaffold-score (grid/parse example-grid))
(scaffold-score (grid/parse (robot-grid)))

;; now we forget all about that.
;; we don't try any solving or whatever.
;; we just walk straight forward and turn when we have to.

(defn start-position [grid]
  (->> (grid/coords grid)
       (filter (fn [c] (= (grid/at grid c) \^)))
       (first)))

(start-position (grid/parse (robot-grid)))

(def example-grid2
  '("#######...#####"
    "#.....#...#...#"
    "#.....#...#...#"
    "......#...#...#"
    "......#...###.#"
    "......#.....#.#"
    "^########...#.#"
    "......#.#...#.#"
    "......#########"
    "........#...#.."
    "....#########.."
    "....#...#......"
    "....#...#......"
    "....#...#......"
    "....#####......"))

(defn move [grid [x y] dir]
  (let [next-pos (mapv + [x y] (case dir :up [0 -1] :down [0 1] :left [-1 0] :right [1 0]))
        ;; _ (println next-pos (grid/in-bounds? grid next-pos) (grid/at grid next-pos))
        ]
    (if (and (grid/in-bounds? grid next-pos)
             (= (grid/at grid next-pos) \#))
      next-pos
      nil)))

(defn apply-turn [turn-direction dir]
  (match [turn-direction dir]
    [:L :up] :left
    [:L :left] :down
    [:L :down] :right
    [:L :right] :up
    [:R :up] :right
    [:R :left] :up
    [:R :down] :left
    [:R :right] :down))

(defn turn [grid [x y] dir]
  ;; so the idea here is that we can't go straight.
  ;; so, we want to know if we turn left / right, can we go straight?
  (->> '(:L :R)
       (filter
        (fn [turn-direction]
          (move grid [x y] (apply-turn turn-direction dir))))
       (first)))

;; we won't be smart.  we're just going to go straight.
(defn path [grid]
  (loop
   [[x y] (start-position grid)
    dir :up
    result []]
    (println [x y] dir)
    (if-let [[x y] (move grid [x y] dir)]
      (recur [x y] dir (conj result 1))
      (if-let [turn-direction (turn grid [x y] dir)]
        (recur [x y] (apply-turn turn-direction dir) (conj result turn-direction))
        result))))

(defn robot-instructions [grid]
  (->> (path grid)
       (partition-by identity)
       (map (fn [c]
              (case (count c) 1
                    (case (first c) :L \L :R \R)
                    (count c))))
       (string/join ",")))

(robot-instructions (grid/parse example-grid2))

;; final thing we need to do is combine it into sub-instructions
;; that seems kinda annoying, I'll do it tomorrow.
