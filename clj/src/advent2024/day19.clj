(ns advent2024.day19
  (:require
    [utils :as utils]))

(def example
'("r, wr, b, g, bwu, rb, gb, br"
  ""
  "brwrr"
  "bggr"
  "gbbr"
  "rrbgbr"
  "ubwu"
  "bwurrg"
  "brgr"
  "bbrgwb"))

(defn parse-towels [lines]
  (let [[^String patterns _ & r] lines]
    [(.split #", " patterns) r]))

(parse-towels example)

;; recursive memoization
(def can-make?
  (memoize (fn [patterns str]
             (if (empty? str)
               true
               (->> patterns
                    (filter #(.startsWith str %))
                    (map #(subs str (count %)))
                    (filter (partial can-make? patterns))
                    (seq))))))

(defn answer-part1 [lines]
  (let [[patterns towels] (parse-towels lines)]
    (count (filter (partial can-make? (sort-by count > patterns)) towels))))

(answer-part1 example)
(time (answer-part1 (utils/read-input "2024/day19.txt")))

(def towel-count
  (memoize
   (fn [patterns str]
     (if (empty? str)
       1
       (->> patterns
            (filter #(.startsWith str %))
            (map #(subs str (count %)))
            (map (partial towel-count patterns))
            (reduce +))))))

(towel-count ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"] "rrbgbr")

(defn answer-part2 [lines]
  (let [[patterns towels] (parse-towels lines)]
    (reduce + (map (partial towel-count (sort-by count > patterns)) towels))))

(answer-part2 example)
(answer-part2 (utils/read-input "2024/day19.txt"))
