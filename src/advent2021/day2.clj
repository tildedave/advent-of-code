(ns advent2021.day2
  (:require [advent2021.utils :as utils]))

(re-matches #"^Forward (\d)+" "babnana")

(defn parse-line [line]
 (let [m (re-matches #"^forward (\d+)$" line)
       m2 (re-matches #"^down (\d+)$" line)
       m3 (re-matches #"^up (\d+)$" line)]
   (cond m [:forward (utils/parse-int (second m))]
         m2 [:down (utils/parse-int (second m2))]
         m3 [:up (utils/parse-int (second m3))])))

(defn answer-part1 [lines]
  (->> lines
       (map parse-line)
       (reduce
        (fn [[x depth] parsed-line]
          (let [n (second parsed-line)]
            (case
             (first parsed-line)
              :forward [(+ x n) depth]
              :down [x (+ depth n)]
              :up [x (- depth n)])))
        [0 0])
       (reduce *)))

(defn answer-part2 [lines]
   (->> lines
       (map parse-line)
       (reduce
        (fn [[x depth aim] parsed-line]
           (let [n (second parsed-line)]
             (case
              (first parsed-line)
               :forward [(+ x n) (+ depth (* aim n)) aim]
               :down [x depth (+ aim n)]
               :up [x depth (- aim n)])))
        [0 0 0])
        (take 2)
        (reduce *)))

(answer-part1 (utils/read-resource-lines "input/day2-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day2.txt"))

(answer-part2 (utils/read-resource-lines "input/day2-example.txt"))
(answer-part2 (utils/read-resource-lines "input/day2.txt"))
