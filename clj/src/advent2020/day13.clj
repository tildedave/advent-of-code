(ns advent2020.day13
  (:require [utils :as utils]
            [clojure.string :as string]))

(defn parse-input [lines]
  (let [[timestamp buses] (utils/read-input (format "2020/%s" lines))]
    [(utils/parse-int timestamp)
     (->> (string/split buses #",")
          (map utils/try-parse-int))]))

(defn answer-part1 [lines]
  (let [[n moduli] (parse-input lines)]
    (->> moduli
         (remove (partial = "x"))
         (map #(vector % (- % (mod n %))))
         (sort-by second)
         (first)
         (reduce *))))

(answer-part1 "day13-example.txt")
(answer-part1 "day13.txt")

;; so obvious part2 is the CRT.  CRT is life

(defn answer-part2 [lines]
  (let [[_ moduli] (parse-input lines)]
    (->> moduli
         (map-indexed vector)
         (remove #(= "x" (second %)))
         (map (fn [[n x]] [(- x n) x]))
         (utils/crt-inductive)
         )))

(answer-part2 "day13-example.txt")
(answer-part2 "day13.txt")
