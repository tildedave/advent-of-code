(ns advent2015.day1
  (:require [utils :as utils]))

(defn read-filename [filename]
  (->> filename
       (format "2015/%s")
       (utils/read-input)
       (first)
       (seq)))

(defn answer-part1 [filename]
  (reduce
   (fn [floor ch]
     (case ch
       \( (inc floor)
       \) (dec floor)))
   0
   (read-filename filename)
   ))

(defn answer-part2 [filename]
  (reduce
   (fn [floor [n ch]]
     (if (= floor -1)
       (reduced n)
       (case ch
         \( (inc floor)
         \) (dec floor))))
   0
   (map-indexed vector (read-filename filename))))

(answer-part1 "day1.txt")
(answer-part2 "day1.txt")
(answer-part2 "day1-example.txt")
