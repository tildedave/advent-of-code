(ns advent2024.day13
  (:require [clojure.math :as math]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as ml]
            [utils :as utils]))

;; good ol'fashioned linear algebra

(def q [[94 22] [34 67]])

;; (ml/solve q [8400 5400])

;; (ml/solve [[26 67] [66 21]] [12748 12176])

(m/det [[26 67] [66 21]])
(m/det [[94 22] [34 67]])

(utils/str->nums "Button A: X+94, Y+34")

(defn parse-buttons [[line1 line2 line3]]
  (let [[x1 y1] (utils/str->nums line1)
        [x2 y2] (utils/str->nums line2)
        [a b] (utils/str->nums line3)
        double-result (ml/solve (m/matrix :vectorz [[x1 x2] [y1 y2]]) [a b])
        [t1 t2] (map #(math/round %) double-result)]
    (if
     (and
     (= a (+ (* x1 t1) (* x2 t2)))
     (= b (+ (* y1 t1) (* y2 t2)))
      (<= t1 100)
      (<= t2 100)
      )
      (+ (* 3 t1) t2)
      0)))

(def example
  '("Button A: X+94, Y+34"
    "Button B: X+22, Y+67"
    "Prize: X=8400, Y=5400"
    ""
    "Button A: X+26, Y+66"
    "Button B: X+67, Y+21"
    "Prize: X=12748, Y=12176"
    ""
    "Button A: X+17, Y+86"
    "Button B: X+84, Y+37"
    "Prize: X=7870, Y=6450"
    ""
    "Button A: X+69, Y+23"
    "Button B: X+27, Y+71"
    "Prize: X=18641, Y=10279"))

(map parse-buttons (utils/split-by "" example))

(parse-buttons '("Button A: X+94, Y+34"
                  "Button B: X+22, Y+67"
                  "Prize: X=8400, Y=5400"))

(parse-buttons '("Button A: X+26, Y+66"
"Button B: X+67, Y+21"
"Prize: X=12748, Y=12176"))

(utils/read-input "2024/day13.txt")
