(ns advent2023.day11
  (:require [grid :as grid]
            [clojure.set :as set]))

;; so I guess this is fairly easy, it is just taxicab metric
;; with a fudge.

(def example-map
  '("...#......"
    ".......#.."
    "#........."
    ".........."
    "......#..."
    ".#........"
    ".........#"
    ".........."
    ".......#.."
    "#...#....."))

(defn all-empty? [grid coord-list]
  (every? #(= \. (grid/at grid %)) coord-list))

(defn empty-rows [grid]
  (->> (group-by second (grid/coords grid))
       (filter (fn [[_ coord-list]] (all-empty? grid coord-list)))
       (map first)
       (set)))

(defn empty-columns [grid]
  (->> (group-by first (grid/coords grid))
       (filter (fn [[_ coord-list]] (all-empty? grid coord-list)))
       (map first)
       (set)))

(empty-rows (grid/parse example-map))
(empty-columns (grid/parse example-map))

(defn galaxy-coords [grid]
  (->> (grid/coords grid)
       (filter #(= \# (grid/at grid %)))))

(defn distance-between
  [empty-columns
   empty-rows
   column-expansion-factor
   row-expansion-factor
   [gx1 gy1]
   [gx2 gy2]]
  (+
   (+
    (* (count (set/intersection empty-columns (set (range (min gx1 gx2) (inc (max gx1 gx2))))))
       (dec column-expansion-factor))
    (abs (- gx1 gx2)))
   (+
    (* (count (set/intersection empty-rows (set (range (min gy1 gy2) (inc (max gy1 gy2))))))
       (dec row-expansion-factor))
    (abs (- gy1 gy2)))))

(defn answer [grid column-expansion row-expansion]
  (let [empty-rows (empty-rows grid)
        empty-columns (empty-columns grid)
        coords (galaxy-coords grid)]
    (as->
     (for [g1 coords
           g2 coords]
       (if (= g1 g2)
         nil
         (distance-between empty-columns empty-rows column-expansion row-expansion g1 g2)))
     g
      (remove nil? g)
      (reduce + g)
      (/ g 2))))

(answer (grid/parse example-map) 2 2)
(answer (grid/parse-file "2023/day11.txt") 2 2)

;; easy
(answer (grid/parse example-map) 10 10)
(answer (grid/parse example-map) 100 100)
(answer (grid/parse-file "2023/day11.txt") 1000000 1000000)
