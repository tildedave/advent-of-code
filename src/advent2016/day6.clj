(ns advent2016.day6
  (:require [clojure.string :as string]
            [utils :as utils]))

(defn answer [lines part2?]
  (let [len-one (count (first lines))]
  (->> lines
       (map seq)
       (apply interleave)
       (partition (count lines))
       (map #(map (fn [ch] {ch 1}) %))
       (map #(apply merge-with + %))
       (map #(sort-by second (if part2? < >) %))
       (map first)
       (map first)
       (string/join)
       )))

(defn answer-part1 [lines] (answer lines false))
(defn answer-part2 [lines] (answer lines true))
(answer-part1
 ["eedadn"
  "drvtee"
  "eandsr"
  "raavrd"
  "atevrs"
  "tsrnev"
  "sdttsa"
  "rasrtv"
  "nssdts"
  "ntnada"
  "svetve"
  "tesnvt"
  "vntsnd"
  "vrdear"
  "dvrsen"
  "enarar"])

(answer-part1 (utils/read-input "2016/day6.txt"))

(answer-part2 (utils/read-input "2016/day6.txt"))
