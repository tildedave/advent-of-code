(ns advent2015.day3
  (:require [utils :as utils]))

(defn step [[houses-so-far [x y]] ch]
  (let [[nx ny] (case ch
                  \^ [x (dec y)]
                  \v [x (inc y)]
                  \> [(inc x) y]
                  \< [(dec x) y])]
    [(update houses-so-far [nx ny] (fnil inc 0))
     [nx ny]]))

(defn visit [instructions]
  (reduce
   step
   [{[0 0] 1} [0 0]]
   (seq instructions)))

(defn answer-part1 [instructions]
  (->> (visit instructions)
       (first)
       (keys)
       (count)))

(answer-part1 "^>v<")
(answer-part1 (first (utils/read-input "2015/day3.txt")))

(defn visit-p2 [instructions]
  (reduce
   (fn [[houses-so-far [sx sy] [rx ry] n] ch]
     (if (= (mod n 2) 0)
       (let [[houses-so-far [sx sy]] (step [houses-so-far [sx sy]] ch)]
         [houses-so-far [sx sy] [rx ry] (inc n)])
       (let [[houses-so-far [rx ry]] (step [houses-so-far [rx ry]] ch)]
         [houses-so-far [sx sy] [rx ry] (inc n)])))
   [{[0 0] 2} [0 0] [0 0] 0]
   (seq instructions)))


(defn answer-part2 [instructions]
  (->> (visit-p2 instructions)
       (first)
       (keys)
       (count)))
