(ns advent2016.day13
  (:require [grid :as grid]
            [utils :as utils]
            [clojure.data.priority-map :refer [priority-map]]))

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
  (grid/a*-search
   [1 1]
   (fn [[x y]] (and (= x gx) (= y gy)))
   (neighbors (is-wall? puzzle-input))
   (heuristic [gx gy])
   (fn [_ _] 1)))

(defn puzzle-input []
  (utils/parse-int (first (utils/read-input "2016/day13.txt"))))

(answer 10 [7 4])
(answer (puzzle-input) [31 39])


;; unfortunately I have to actually code dijsktra's algo here.

(defn num-locations [neighbors]
  (let [start [1 1]]
    (loop [[visited distance queue] [#{} {start 0} (priority-map start 0)]]
      (if (empty? queue)
        visited
        (let [[current dist] (peek queue)
              queue (pop queue)
              visited (conj visited current)]
          (if (= dist 50)
            (recur [visited distance queue])
            (recur
             (reduce
              (fn [[visited distance queue] neighbor]
                (if
                 (contains? visited neighbor) [visited distance queue]
                 (let [alt (inc (distance current))]
                   (if (< alt (get distance neighbor Integer/MAX_VALUE))
                     [visited (assoc distance neighbor alt) (assoc queue neighbor alt)]
                     [visited distance queue]))))
              [visited distance queue]
              (neighbors current)))))))))

;; unfortunately I coded it 99% correctly the first try.

(count (num-locations (neighbors (is-wall? (puzzle-input)))))
