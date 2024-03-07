(ns advent2022.day2
  (:require [advent2022.utils :as utils]
            [clojure.set :as set]))

(def lines (utils/read-resource-lines "input/day2.txt"))

(def winning-map {"X" "Z", "Y" "X", "Z"  "Y"})

(def losing-map (set/map-invert winning-map))

;; X rock
;; Y paper
;; Z scissors
(defn winning-score
  "Returns 6 if p1 (I) win, 0 if p2 wins, 3 if tie"
  [p1 p2]
  (cond (= p1 p2) 3
        (= (winning-map p1) p2) 6
        (= (winning-map p2) p1) 0))

;; there's a cooler way to do this where we use the data in the above function to avoid duplicating the information.

;; X rock
;; Y paper
;; Z scissors
(defn desired-shape [result shape]
  (cond (= result "Y") shape
        (= result "X") (winning-map shape)
        (= result "Z") (losing-map shape)))

(def prediction-map {"A" "X", "B" "Y", "C" "Z"})

(defn shape-points [shape]
  (case shape "X" 1 "Y" 2 "Z" 3 -1))

(def game-map (map #(.split % " ") lines))

(defn game-score-p1 [prediction shape]
  (+ (shape-points shape)
     (winning-score shape
                    (prediction-map prediction))))

;; answer for part 1
(reduce + (map (partial apply game-score-p1) game-map))

(defn game-score-p2 [prediction result]
  (let [opp-shape (prediction-map prediction)
        shape (desired-shape result opp-shape)]
    (+ (shape-points shape)
       (winning-score shape opp-shape))))

;; answer for part 2
(reduce + (map (partial apply game-score-p2) game-map))
