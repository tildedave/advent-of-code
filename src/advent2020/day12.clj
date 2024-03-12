(ns advent2020.day12
  (:require [utils :as utils]))

(re-matches #"(N|S|E|W|L|R|F)(\d+)" "F7")

(defn parse-directive [line]
  (let [[_ command num-str] (re-matches #"^(N|S|E|W|L|R|F)(\d+)$" line)]
    [(case command
       "N" :north
       "S" :south
       "E" :east
       "W" :west
       "L" :left
       "R" :right
       "F" :forward)
     (utils/parse-int num-str)]))

;; (0 -1
;;  1 0)  -> rotate right

;; (0 1
;;  -1 0) -> rotate left

(defn rotate [[xv yv] rotate-direction]
  (map
   (fn [[a b]] (+ (* xv a) (* yv b)))
   (case rotate-direction
     :left
     [[0 -1] [1 0]]
     case :right
     [[0 1] [-1 0]])))

(rotate [0 1] :right)

(defn step [[x y dir] [command n]]
  (case command
    :north [x (+ y n) dir]
    :south [x (- y n) dir]
    :east [(+ x n) y dir]
    :west [(- x n) y dir]
    :left [x y (rotate dir :left)]
    :right [x y (rotate dir :right)]
    :forward (let [[xv yv] dir]
               [(+ x (* xv n)) (+ y (* yv n)) dir])))

(defn answer-part1 [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (map parse-directive)
       (reduce step [0 0 [1 0]])
       (take 2)
       (map abs)
       (reduce +)))

(answer-part1 "day12-example.txt")
(answer-part1 "day12.txt")


;; (let [l (->> (utils/read-input "2020/day12-example.txt")
;;              (map parse-directive))]
;;   (iterate (fn [[state l]]
;;              (if (empty? l)
;;                [state l]
;;                [(step state (first l)) (rest l)]))
;;            [[0 0 [1 0]] l]))
