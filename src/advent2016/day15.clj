(ns advent2016.day15
  (:require [utils :as utils]))

;; so obviously this is just CRT

(def line-re #"^Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)\.$")

(defn parse-line [s]
  (->> s
       (re-matches line-re)
       (rest)
       (map utils/parse-int)
       ((fn [[d p offset]] [(mod (- (+ d offset)) p) p]))))

(defn answer [filename]
  (->> (utils/read-input filename)
       (map parse-line)
       (utils/crt-inductive)))

(answer "2016/day15.txt")

(defn answer-part2 []
  (let [lines (utils/read-input "2016/day15.txt")]
    (->> (conj lines (format "Disc #%d has 11 positions; at time=0, it is at position 0." (inc (count lines))))
         (map parse-line)
         (utils/crt-inductive))))

(answer-part2)
