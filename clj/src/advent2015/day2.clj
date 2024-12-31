(ns advent2015.day2
  (:require [utils :as utils]))

(defn wrapping-paper-needed [l w h]
  (+
   (+ (* 2 l w)
     (* 2 l h)
     (* 2 w h))
   (reduce min [(* l w) (* l h) (* w h)])))


(assert (= (wrapping-paper-needed 2 3 4) 58))
(assert (= (wrapping-paper-needed 1 1 10) 43))

(defn parse-line [line]
  (->> (.split line "x")
       (map utils/parse-int)))

(defn read-input [filename]
  (->> filename
       (format "2015/%s")
       (utils/read-input)
       (map parse-line)))

(defn answer-part1 [filename]
  (->> (read-input filename)
       (map #(apply wrapping-paper-needed %))
       (reduce +)))

(answer-part1 "day2.txt")

(defn ribbon-needed [l w h]
  (->> (list l w h)
       (sort)
       (take 2)
       (reduce +)
       (* 2)
       (+ (* l w h))))

(assert (= (ribbon-needed 10 1 1) 14))
(assert (= (ribbon-needed 2 3 4) 34))

(defn answer-part2 [filename]
  (->> (read-input filename)
       (map #(apply ribbon-needed %))
       (reduce +)))

(answer-part2 "day2.txt")
