(ns advent2016.day13
  (:require [graph :as graph]
            [utils :as utils]))

(defn is-wall? [puzzle-input]
  (memoize
   (fn [[x y]]
     (let [num (+ (* x x) (* 3 x) (* 2 x y) y (* y y) puzzle-input)]
       (= (mod (->> (Integer/toString num 2)
                    (seq)
                    (filter (partial = \1))
                    (count)) 2)
          1)))))


(assert (true? ((is-wall? 10) [1 0])))
(assert (false? ((is-wall? 10) [0 1])))

(defn neighbors [is-wall?]
  (memoize
   (fn [[x y]]
     (->>
      (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]]
        (let [[nx ny] [(+ x dx) (+ y dy)]]
          (if (or (< nx 0) (< ny 0))
            nil
            [nx ny])))
      (remove nil?)
      (remove is-wall?)))))

((neighbors (is-wall? 10)) [4 2])

(defn heuristic [[gx gy]]
  (fn [[x y]]
  ;; taxicab distance to our goal
    (+ (abs (- gx x)) (abs (- gy y)))))

(defn answer [puzzle-input [gx gy]]
  (first (graph/a*-search
   [1 1]
   (fn [[x y]] (and (= x gx) (= y gy)))
   (neighbors (is-wall? puzzle-input))
   (heuristic [gx gy])
   (fn [_ _] 1))))

(defn puzzle-input []
  (utils/parse-int (first (utils/read-input "2016/day13.txt"))))

(answer 10 [7 4])
(answer (puzzle-input) [31 39])


;; unfortunately I have to actually code dijsktra's algo here.

;; unfortunately I coded it 99% correctly the first try.

(count
 (graph/dijkstra-search
  [1 1]
  (neighbors (is-wall? (puzzle-input)))
  (fn [_ dist] (= dist 50))))
