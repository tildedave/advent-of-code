(ns advent2020.day6
  (:require [utils :as utils]
            [clojure.set :as set]))

(utils/split-by "" (utils/read-input "2020/day6-example.txt"))

(defn answer-part1 [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (utils/split-by "")
       (map #(map seq %))
       (map
        #(reduce
          (partial reduce conj)
          #{} %))
       (map count)
       (reduce +)))

(answer-part1 "day6-example.txt")
(answer-part1 "day6.txt")

(defn answer-part2 [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (utils/split-by "")
       (map #(map (comp set seq) %))
       (map #(reduce set/intersection (first %) (rest %)))
       (map count)
       (reduce +)
       ))

(answer-part2 "day6-example.txt")
(answer-part2 "day6.txt")
