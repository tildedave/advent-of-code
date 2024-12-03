(ns advent2024.day3
  (:require [utils :as utils]))

(def example-line "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn total-line [line]
  (->> line
       (re-seq #"mul\((\d+),(\d+)\)")
       (map #(reduce * (map utils/parse-int (rest %))))
       (reduce +)))

(->> (utils/read-input "2024/day3.txt")
     (map total-line)
     (reduce +))
