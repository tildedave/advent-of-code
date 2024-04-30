(ns advent2016.day20
  (:require [utils :as utils]
            [intervals :as intervals]))

;; this is an interval tree

(set! *warn-on-reflection* true)

(defn parse-line [^String line]
  (map parse-long (.split line "-")))

(parse-line "5-8")

(conj '(1 2 3 4) 5)

(defn answer-part1 [filename]
  (let [block-list (intervals/interval-list-merge (->> filename
                                                       (utils/read-input)
                                                       (map parse-line)))]
    (->> (map (fn [[lo1 hi1] [lo2 hi2]]
           (if (= hi1 (dec lo2)) 0
               (inc hi1)))
         block-list
         (rest block-list))
         (remove zero?)
         (first))))

(answer-part1 "2016/day20.txt")

(defn answer-part2 [filename m]
  (let [block-list (intervals/interval-list-merge (->> filename
                                                       (utils/read-input)
                                      (map parse-line)))]
    (reduce +
            (map (fn [[lo1 hi1] [lo2 hi2]]
           (dec (- lo2 hi1)))
         block-list (concat (rest block-list) [[(inc m) (inc m)]])
     ))))

(answer-part2 "2016/day20.txt" 4294967295)

