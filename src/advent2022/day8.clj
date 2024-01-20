(ns advent2022.day8
  (:require [advent2022.utils :as utils]
            [clojure.string :as string]))

(def lines (utils/read-resource-lines "input/day8-example.txt"))
(def num-columns (count (first lines)))
(def num-rows (count lines))
(def grid (mapv #(Character/getNumericValue %) (string/join lines)))
(defn tree-height-at [x y] (nth grid (+ (* y num-rows) x)))


;;; need to create a data structure that, for each point in the grid,
;;; has the max distance north/south/east/west.

;;; feels like the coolest way to do this is some sort of recursive lazy data
;;; structure.  I guess we can just try a basic memoized recursive function.

;;; max tree in X direction from x, y - not including itself.
(def max-tree
  (memoize
   (fn [x y direction]
     (case direction
       :north
       (if
        (= y 0) -1
        (max (tree-height-at x (dec y)) (max-tree x (dec y) :north)))
       :south
       (if
        (= y (dec num-rows)) -1
        (max (tree-height-at x (inc y)) (max-tree x (inc y) :south)))
       :east
       (if
        (= x (dec num-columns)) -1
        (max (tree-height-at (inc x) y) (max-tree (inc x) y :east)))
       :west
       (if
        (= x 0) -1
        (max (tree-height-at (dec x) y) (max-tree (dec x) y :west)))))))

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

 (tree-height-at 3 0)
 (max-tree 3 1 :north)
