(ns advent2020.day1
  (:require [advent2020.utils :as utils]))


(defn answer [filename n])

(defn answer-part1 [filename]
  (let [l (->> (utils/read-input filename)
               (map #(Integer/valueOf %)))]
    (->> (for [x l y l] [x y])
         (filter #(< (first %) (second %)))
         (filter #(= 2020 (+ (first %) (second %))))
         (first)
         (reduce *))))

(defn answer-part2 [filename]
(let [l (->> (utils/read-input filename)
              (map #(Integer/valueOf %)))]
   (->> (for [x l y l z l] [x y z])
        (filter #(apply < %))
        (filter #(= 2020 (apply + %)))
        (first)
        (reduce *))))

(answer-part2 "day1.txt")
