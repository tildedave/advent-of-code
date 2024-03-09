(ns advent2020.day5
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [utils :as utils]))

(defn binarify [ch]
  (case ch \F 0 \B 1 \R 1 \L 0))

(partition-all 7 "FFBFFFBLLL")

(Integer/valueOf "0101100" 2)
(defn seat-id [line]
  (->> line
       (seq)
       (map binarify)
       (partition-all 7)
       (map (partial string/join ""))
       (map #(Integer/parseInt % 2))
       (#(+ (* (first %) 8) (second %)))))

(defn answer-part1 [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (map seat-id)
       (reduce max)))

(answer-part1 "day5-example.txt")
(answer-part1 "day5.txt")

(defn answer-part2 [filename]
  (let [sorted-list (->> (utils/read-input (format "2020/%s" filename))
                         (map seat-id)
                         (sort))]
    (->>
     (map (fn [n m o]
            [n (and (= m (inc n)) (= o (inc m)))])
         sorted-list (rest sorted-list) (rest (rest sorted-list)))
     (remove #(second %))
     (second)
     (first)
     (inc))))

(answer-part2 "day5.txt")
