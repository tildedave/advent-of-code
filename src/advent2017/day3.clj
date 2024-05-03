(ns advent2017.day3
  (:require [utils :as utils]
            [grid :as grid]))

;; 1 4 8 15 23 34 46
;; "turning points"
;; 2 10 26 50
(- 50 26)
;; 37 36  35  34  33 32 31
;; 38 17  16  15  14  13 30
;; 39 18   5   4   3  12 29
;; 40 19   6   1   2  11 28
;; 41 20   7   8   9  10 27
;; 42 21  22  23  24  25 26
;; 43 44  45  46  47  48 49  50

(def initial-state [[0 0] :right 0 1])

(defn step [[[x y] direction spiral-length steps-left]]
  (let [[dx dy] (case direction :right [1 0] :up [0 -1] :left [-1 0] :down [0 1])
        steps-left (dec steps-left)
        change-direction? (= steps-left 0)
        next-direction (if change-direction?
                         (case direction
                           :right :up
                           :up :left
                           :left :down
                           :down :right)
                         direction)
        new-spiral? (and change-direction? (= next-direction :up))
        spiral-length (if new-spiral?
                        (+ spiral-length 2)
                        spiral-length)
        steps-left (cond
                     new-spiral? (dec spiral-length)
                     (and change-direction? (= next-direction :right)) (inc spiral-length)
                     change-direction? spiral-length
                     :else steps-left)]
    [[(+ x dx) (+ y dy)] next-direction spiral-length steps-left]))

(def ride-the-spiral (iterate step initial-state))

(defn answer [n]
  (let [[x y] (first (nth ride-the-spiral (dec n)))]
    (+ (abs x) (abs y))))

(answer (utils/parse-int (first (utils/read-input "2017/day3.txt"))))

(reductions
 (fn [acc [[x y]]]
   (assoc acc [x y]
          (->> (for [[dx dy] grid/all-directions]
                 (get acc [(+ x dx) (+ y dy)] 0))
               (reduce +))))
 {[0 0] 1}
 ride-the-spiral)
