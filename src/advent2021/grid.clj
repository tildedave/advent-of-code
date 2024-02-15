(ns advent2021.grid
  (:require [advent2021.utils :as utils]))

(defn parse [lines sq-f]
  (->> lines
       (map seq)
       (map #(mapv sq-f %))
       (vec)))

(defn bounds [grid]
  [(count (first grid))
   (count grid)])

(defn out-of-bounds? [grid [x y]]
  (or (< y 0)
      (>= y (count grid))
      (< x 0) (>= x (count (first grid)))))

(defn neighbors [grid [x y]]
  (remove
   nil?
   (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]]
     (let [[nx ny] [(+ x dx) (+ y dy)]]
       (if (out-of-bounds? grid [nx ny])
         nil
         [nx ny])))))

(defn coords [grid]
  (let [xmax (count (first grid))
        ymax (count grid)]
    (for [x (range 0 xmax)
          y (range 0 ymax)]
      [x y])))
