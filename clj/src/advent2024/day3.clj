(ns advent2024.day3
  (:require [utils :as utils]
            [clojure.java.io :as io]))

(def example-line "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn total-line [line]
  (->> line
       (re-seq #"mul\((\d+),(\d+)\)")
       (map #(reduce * (map utils/parse-int (rest %))))
       (reduce +)))

(->> (utils/read-input "2024/day3.txt")
     (map total-line)
     (reduce +))

(defn total-line-part2
  ([line] (total-line-part2 [:do 0] line))
  ([start line]
   (->> line
        (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)")
        (reduce
         (fn [[status acc] [directive & args]]
           (case directive
             "don't()" [:dont acc]
             "do()" [:do acc]
             (case status
               :do [status (+ acc (reduce * (map utils/parse-int args)))]
               :dont [status acc])))
         start))))

(def example-line-part2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(total-line-part2 example-line-part2)

(->> (slurp (io/reader (io/resource "2024/day3.txt")))
     (total-line-part2 [:do 0])
     (second))
