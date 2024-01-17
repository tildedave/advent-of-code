(ns advent2022.day2
  (:require [clojure.java.io :as io]))

(def lines
  (line-seq (io/reader (io/resource "input/day2.txt"))))

;; X rock
;; Y paper
;; Z scissors
(defn winning-score
  "Returns 6 if p1 (I) win, 0 if p2 wins, 3 if tie"
  [p1 p2]
  (case [p1 p2]
    (["X" "X"]) 3
    (["X" "Y"]) 0
    (["X" "Z"]) 6
    (["Y" "X"]) 6
    (["Y" "Y"]) 3
    (["Y" "Z"]) 0
    (["Z" "X"]) 0
    (["Z" "Y"]) 6
    (["Z" "Z"]) 3
    -10000))

;; there's a cooler way to do this where we use the data in the above function to avoid duplicating the information.

;; X rock
;; Y paper
;; Z scissors
(defn desired-shape [result shape]
  (if (= result "Y") shape
    (case [result shape]
    ;; X - we need to lose
      (["X" "X"]) "Z"
      (["X" "Y"]) "X"
      (["X" "Z"]) "Y"
      (["Z" "X"]) "Y"
      (["Z" "Y"]) "Z"
      (["Z" "Z"]) "X")))

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
