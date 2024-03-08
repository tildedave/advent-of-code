(ns advent2020.day1
  (:require [advent2020.utils :as utils]
            [clojure.set :as set]))


(defn answer [filename n])

(defn subset-sum [l val]
  (set/intersection (set l) (->> l (map #(- val %)) (set))))

(defn answer-part1 [filename]
  (let [l (->> (utils/read-input filename)
               (map #(Integer/valueOf %)))]
    (reduce * (subset-sum l 2020))))

(answer-part1 "day1-example.txt")
(answer-part1 "day1.txt")

(defn answer-part2 [filename]
  (let [l (->> (utils/read-input filename)
               (map #(Integer/valueOf %)))]
     (->>
      (for [x l]
       [x (subset-sum l (- 2020 x))])
      (remove #(empty? (second %)))
      (first)
      (#(* (first %) (reduce * (second %))))
      )))

(answer-part2 "day1-example.txt")
(answer-part2 "day1.txt")
