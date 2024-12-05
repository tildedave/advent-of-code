(ns advent2024.day5
  (:require
   [utils :as utils]
   [clojure.set :as set]))

(def example-lines
  '("47|53"
    "97|13"
    "97|61"
    "97|47"
    "75|29"
    "61|13"
    "75|53"
    "29|13"
    "97|29"
    "53|29"
    "61|53"
    "97|53"
    "61|29"
    "47|13"
    "75|47"
    "97|75"
    "47|61"
    "75|61"
    "47|29"
    "75|13"
    "53|13"
    ""
    "75,47,61,53,29"
    "97,61,53,29,13"
    "75,29,13"
    "75,97,47,61,53"
    "61,13,29"
    "97,13,75,29,47"))

(defn parse-orderings [lines]
  (->> lines
       (map (fn [s] (->> s (.split #"\|") (map utils/parse-int) ((fn [[x y]] {x #{y}})))))
       (apply merge-with set/union)))

(defn parse-line [line]
  (map utils/parse-int (.split #"," line)))

(defn sort-line [orderings num-list]
    (->> num-list (sort (fn [n1 n2]
                 (cond
                 (contains? (get orderings n1 #{}) n2)
                 -1
                 (contains? (get orderings n2 #{}) n1)
                 1
                 :else 0)))))

(defn middle [num-list]
  (let [len (count num-list)]
    (assert (= (mod len 2) 1))
    (nth num-list (quot (dec len) 2))))

(defn answer-part1 [lines]
  (let [[ordering-lines number-lines] (utils/split-by "" lines)
      orderings (parse-orderings ordering-lines)]
  (->> number-lines
       (map parse-line)
       (filter #(= % (sort-line orderings %)))
       (map middle)
       (reduce +))))

(answer-part1 example-lines)
(answer-part1 (utils/read-input "2024/day5.txt"))

(defn answer-part2 [lines]
  (let [[ordering-lines number-lines] (utils/split-by "" lines)
        orderings (parse-orderings ordering-lines)]
    (->> number-lines
         (map parse-line)
         (remove #(= % (sort-line orderings %)))
         (map (partial sort-line orderings))
         (map middle)
         (reduce +))))

(answer-part2 example-lines)
(answer-part2 (utils/read-input "2024/day5.txt"))
