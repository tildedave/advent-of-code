(ns advent2023.day6
  (:require [utils :as utils]))

(defn boat-distance [speed num-seconds]
  (* speed num-seconds))

(defn ways-to-beat-record [num-seconds distance]
  (->>
   (range num-seconds)
   (map (fn [n] (boat-distance n (- num-seconds n))))
   (drop-while #(<= % distance))
   (take-while #(> % distance))
   (count)
  ))

(ways-to-beat-record 30 200)

(defn parse-distance-and-times [lines]
  (->> lines
       (map #(-> (.split #":\s+" % 2)
                 (second)
                 (utils/parse-number-list)))
       (apply map vector)))

(parse-distance-and-times
 '("Time:      7  15   30"
   "Distance:  9  40  200"))

(defn answer-part1 [lines]
  (->> lines
       (parse-distance-and-times)
       (map (partial apply ways-to-beat-record))
       (reduce *)))

(answer-part1
   '("Time:      7  15   30"
     "Distance:  9  40  200"))

(answer-part1 (utils/read-input "2023/day6.txt"))

(defn parse-distance-and-times-part2 [lines]
  (->> lines
       (mapv #(-> (.split #":\s+" % 2)
                 (second)
                 (.replaceAll "\\s+" "")
                 (parse-long)))))

(parse-distance-and-times-part2 '("Time:      7  15   30"
                                  "Distance:  9  40  200"))

;; just pure brute force
;; I made cutting off a bit smarter
(apply ways-to-beat-record (parse-distance-and-times-part2 (utils/read-input "2023/day6.txt")))
