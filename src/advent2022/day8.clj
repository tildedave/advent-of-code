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
(def max-tree-with-distance
  (memoize
   (fn [x y direction]
     (let [at-bounds
           (case direction
             :north (= y 0)
             :south (= y (dec num-rows))
             :west (= x 0)
             :east (= x (dec num-columns)))]
       (if at-bounds [-1 0]
           (let [[nx ny] (case direction
                           :north [x (dec y)]
                           :south [x (inc y)]
                           :east [(inc x) y]
                           :west [(dec x) y])
                 [max-height distance] (max-tree-with-distance nx ny direction)
                 neighbor-height (tree-height-at nx ny)]
                  (if (> max-height neighbor-height)
                    [max-height (inc distance)]
                    [neighbor-height 1])))))))

(max-tree-with-distance 2 1 :north)

(def visibility-grid
  (map
   (fn [idx]
     (let [x (mod idx num-columns)
           y (quot idx num-rows)
           height (tree-height-at x y)]
       (cond
         (> height (first (max-tree-with-distance x y :north))) 1
         (> height (first (max-tree-with-distance x y :south))) 1
         (> height (first (max-tree-with-distance x y :east))) 1
         (> height (first (max-tree-with-distance x y :west))) 1
         :else 0)))
   (range 0 (* num-columns num-rows))))

;; part 1 answer
(reduce + visibility-grid)

;; part 2 is sort of the inverse of part 1.

(def scenic-score
  (map
   (fn [idx]
       (let [x (mod idx num-columns)
             y (quot idx num-rows)]
          (map #(second (max-tree-with-distance x y %)) [:north :west :east :south])))
     (range 0 (* num-columns num-rows))))

(nth scenic-score 17)
;; answer to part 2
(reduce max scenic-score)

 (tree-height-at 3 0)
 (max-tree 3 1 :north)
