(ns advent2015.day12
  (:require [clojure.data.json :as json]
            [utils :as utils]))



(defn sum-numbers [json]
  (cond
    (nil? json) 0
    (string? json) 0
    (number? json) json
    (vector? json) (reduce + (map sum-numbers json))
    (map? json) (reduce + (map (fn [[_ j]] (sum-numbers j)) json))))

(defn answer-part1 [filename]
  (->> (format "2015/%s" filename)
       (utils/read-input)
       (first)
       (json/read-str)
       (sum-numbers)))

(answer-part1 "day12.txt")

(defn sum-numbers-p2 [json]
  (cond
    (nil? json) 0
    (string? json) 0
    (number? json) json
    (vector? json) (reduce + (map sum-numbers-p2 json))
    (map? json)
    (if (-> (vals json) (set) (contains? "red")) 0
        (reduce + (map (fn [[_ j]] (sum-numbers-p2 j)) json)))))

(defn answer-part2 [filename]
  (->> (format "2015/%s" filename)
       (utils/read-input)
       (first)
       (json/read-str)
       (sum-numbers-p2)))

(sum-numbers-p2 [1, {"c" "red","b" 2},3])

(answer-part2 "day12.txt")
