(ns advent2020.day1
  (:require [advent2020.utils :as utils]))


(defn answer-part1 [filename]
  (let [l (->> (utils/read-input filename)
               (map #(Integer/valueOf %)))]
    (->> (for [x l y l] [x y])
         (filter #(not= (first %) (second %)))
         (filter #(= 2020 (* (first %) (second %)))))))

(answer-part1 "day1-example.txt")
