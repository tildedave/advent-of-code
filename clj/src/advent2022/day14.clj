(ns advent2022.day14
  (:require [utils :as utils]))

(def lines (utils/read-resource-lines "input/day14.txt"))

(defn parse-vertices [str]
  (mapv
   #(map utils/parse-int (.split % ","))
   (-> str
       (.split " -> ")
       vec)))

(defn connect-squares [[x1 y1] [x2 y2]]
  (let [[dx dy] [(compare x2 x1) (compare y2 y1)]]
    (loop [acc []
           [x y] [x1 y1]]
      (if (= [x y] [x2 y2]) (conj acc [x y])
          (recur (conj acc [x y])
                 [(+ x dx) (+ y dy)])))))

(defn parse-path [str]
  (loop [acc []
         vertices (parse-vertices str)]
    (let [f (first vertices)
          s (second vertices)]
      (if (nil? s)
        acc
        (recur
        ;; this duplicates the vertices which is sort of OK
         (apply (partial conj acc) (connect-squares f s))
         (subvec vertices 1))))))

(into {} [[1 2] [3 4]])

(defn parse-grid [lines]
  (reduce
   (fn [acc pts]
     (into acc (map #(vector % \#) pts)))
   {}
   (map parse-path lines)))


(defn grid-bounds [grid]
  (let [by-x (vec (sort (map #(first %) (keys grid))))
        by-y (vec (sort (map #(second %) (keys grid))))]
    [[(first by-x) (last by-x)]
     [(first by-y) (last by-y)]]))

;; the boundaries of the grid will be important for understanding
;; if we've fallen into the abyss. but: we don't have to check that
;; while we work on the initial logic.
(defn process-sand [bounds grid]
  (let [[[min-x max-x] [_ max-y]] bounds]
    (if (= (get grid [500 0] \.) \o) [grid true]
        (loop [[sx sy] [500 0]]
    ;; can the sand move down? (down = y + 1)
          (let [down (get grid [sx (inc sy)] \.)
                down-left (get grid [(dec sx) (inc sy)] \.)
                down-right (get grid [(inc sx) (inc sy)] \.)]
            (cond
              (< sx min-x) [grid true]
              (> sx max-x) [grid true]
              (> sy max-y) [grid true]
              (= down \.) (recur [sx (inc sy)])
              (= down-left \.) (recur [(dec sx) (inc sy)])
              (= down-right \.) (recur [(inc sx) (inc sy)])
              :else
              [(assoc grid [sx sy] \o) false]))))))

(def grid (parse-grid lines))

;; here's our answer to part 1
(let [bounds (grid-bounds grid)]
  (loop [n 0 grid grid]
    (let [[grid stop] (process-sand bounds grid)]
      (if stop
        n
        (recur (inc n) grid)))))

(into {} [[1 2]])
;; here's our answer to part 2
(let [bounds (grid-bounds grid)
      [[min-x max-x] [_ max-y]] bounds
      ;; the end state is a triangle with all the sand piled up.
      ;; it's certain the fudge here is "too much" but it's at least an upper
      ;; bound.
      fudge (+ 2 max-y)
      floor (connect-squares [(- min-x fudge) (+ 2 max-y)] [(+ max-x fudge) (+ 2 max-y)])
      grid-with-floor (into grid (map #(vector % \q) floor))
      bounds (grid-bounds grid-with-floor)]
  (loop [n 0 grid grid-with-floor]
    (let [[grid stop] (process-sand bounds grid)]
      (if stop
        n
        (recur (inc n) grid)))))
