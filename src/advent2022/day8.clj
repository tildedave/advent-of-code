(ns advent2022.day8
  (:require [advent2022.utils :as utils]
            [clojure.string :as string]))

(def lines (utils/read-resource-lines "input/day8.txt"))
(def num-columns (count (first lines)))
(def num-rows (count lines))
(def grid (mapv #(Character/getNumericValue %) (string/join lines)))
(defn tree-height-at [x y] (nth grid (+ (* y num-rows) x)))


;;; need to create a data structure that, for each point in the grid,
;;; has the max distance north/south/east/west.

;;; feels like the coolest way to do this is some sort of recursive lazy data
;;; structure.  I guess we can just try a basic memoized recursive function.

;;; max tree in X direction from x, y - not including itself.

(defn at-bounds? [x y direction]
  (case direction
    :north (= y 0)
    :south (= y (dec num-rows))
    :west (= x 0)
    :east (= x (dec num-columns))))

(defn follow-ray [x y direction]
  (case direction
    :north [x (dec y)]
    :south [x (inc y)]
    :east [(inc x) y]
    :west [(dec x) y]))

(def max-tree
  (memoize
   (fn [x y direction]
     (if (at-bounds? x y direction) -1
         (let [[nx ny] (follow-ray x y direction)
               max-height (max-tree nx ny direction)
               neighbor-height (tree-height-at nx ny)]
           (max max-height neighbor-height))))))

(def visibility-grid
  (map
   (fn [idx]
     (let [x (mod idx num-columns)
           y (quot idx num-rows)
           height (tree-height-at x y)]
       (cond
         (> height (max-tree x y :north)) 1
         (> height (max-tree x y :south)) 1
         (> height (max-tree x y :east)) 1
         (> height (max-tree x y :west)) 1
         :else 0)))
   (range 0 (* num-columns num-rows))))

;; part 1 answer
(reduce + visibility-grid)

;; part 2 is sort of the inverse of part 1.

(defn ray-from [x y direction]
  (if (at-bounds? x y direction) (list)
      (let [[nx ny] (follow-ray x y direction)]
        (cons [nx ny] (ray-from nx ny direction)))))

(defn trees-visible-from [x y direction]
  (let [our-height (tree-height-at x y)]
    (loop [l (ray-from x y direction)
           num 0]
      (if (empty? l) num
          (let [[nx ny] (first l)
                neighbor-height (tree-height-at nx ny)]
            (if (>= neighbor-height our-height) (inc num)
                (recur (rest l) (inc num))))))))

(map #(trees-visible-from 2 3 %) [:north :west :east :south])

(def scenic-score
  (map
   (fn [idx]
     (let [x (mod idx num-columns)
           y (quot idx num-rows)]
       (reduce * (map #(trees-visible-from x y %) [:north :west :east :south]))))
   (range 0 (* num-columns num-rows))))

;; answer to part 2
(reduce max scenic-score)
