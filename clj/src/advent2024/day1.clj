(ns advent2024.day1
  (:require
    [utils :as utils]))

(def example-lines
  '("3   4"
    "4   3"
    "2   5"
    "1   3"
    "3   9"
    "3   3"))

(defn parse-line [line]
  (->> (.split #"\s+" line)
       (map utils/parse-int)))

(utils/read-input "2024/day1.txt")

(map parse-line (utils/read-input "2024/day1.txt"))

(defn answer-part1 [lines]
  (let [parsed-lines (map parse-line lines)
        firsts (sort (mapv first parsed-lines))
        seconds (sort (mapv second parsed-lines))]
    (->> (map vector firsts seconds)
         (map (fn [[x y]] (abs (- y x))))
         (reduce +))))

(answer-part1 (utils/read-input "2024/day1.txt"))
(answer-part1 example-lines)

(defn answer-part2 [lines]
  (let [parsed-lines (map parse-line lines)
        firsts (mapv first parsed-lines)
        freqs (frequencies (mapv second parsed-lines))]
    (->> firsts
         (map #(* % (get freqs % 0)))
         (reduce +))))

(answer-part2 (utils/read-input "2024/day1.txt"))
(answer-part2 example-lines)

