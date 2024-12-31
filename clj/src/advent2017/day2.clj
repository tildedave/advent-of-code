(ns advent2017.day2
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]))

(defn parse-row [s]
  (->> (.split #"\s+" s)
       (map utils/parse-int)))

(defn row-amount [row]
  (- (reduce max row) (reduce min row)))

(defn answer-part1 []
  (->> (utils/read-input "2017/day2.txt")
     (map parse-row)
     (map row-amount)
     (reduce +)))

(answer-part1)

(defn row-divisibility [row]
  (->> (combo/combinations row 2)
       (map (fn [[m n]] (cond
                        (zero? (mod m n)) (quot m n)
                        (zero? (mod n m)) (quot n m)
                        :else 0)))
       (remove zero?)
       (first)))

(row-divisibility '(5 9 2 8))


(defn answer-part2 []
  (->> (utils/read-input "2017/day2.txt")
        (map parse-row)
        (map row-divisibility)
        (reduce +)))

(answer-part2)
