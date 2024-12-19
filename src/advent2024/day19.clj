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
(defn can-make? [patterns]
  (let [patterns (sort-by count > patterns)
        f (fn [rec-f str]
            (if (empty? str)
              true
              (->> patterns
                   (filter #(.startsWith str %))
                   (map #(subs str (count %)))
                   (filter (partial rec-f rec-f))
                   (empty?)
                   (not))))]
    (partial f (memoize f))))

(sort-by count > ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"])

(defn answer-part1 [lines]
  (let [[patterns towels] (parse-towels lines)
        can-make? (can-make? patterns)]
    (count (filter can-make? towels))))

(answer-part1 example)
(answer-part1 (utils/read-input "2024/day19.txt"))

(defn towel-count [patterns]
  (let [patterns (sort-by count > patterns)
        f (fn [rec-f str]
            (if (empty? str)
              1
              (->> patterns
                   (filter #(.startsWith str %))
                   (map #(subs str (count %)))
                   (map (partial rec-f rec-f))
                   (reduce +))))]
    (partial f (memoize f))))

((towel-count ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]) "rrbgbr")

(defn answer-part2 [lines]
  (let [[patterns towels] (parse-towels lines)
        towel-count (towel-count patterns)]
    (reduce + (map towel-count towels))))

(answer-part2 example)
(answer-part2 (utils/read-input "2024/day19.txt"))
