(ns advent2015.day13
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]))

(defn parse-line [line]
  (let [[_ from gain-or-lose units-str to] (re-matches #"^(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).$" line)]
    {from [to (* (case gain-or-lose "gain" +1 -1) (utils/parse-int units-str))]}))

(defn parse-graph [filename]
  (->> filename
       (format "2015/%s")
       (utils/read-input)
       (map parse-line)
       (reduce (partial merge-with concat))
       (#(update-vals % (fn [v] (apply hash-map v))))))

(defn ordering-cost [graph path]
  (let [first-person (first path)]
    (loop [curr (first path)
           path (rest path)
           cost 0]
      (if-let [next (first path)]
        (recur next (rest path) (+ cost
                                   (get-in graph [curr next] 0)
                                   (get-in graph [next curr] 0)
                                   ))
        (+ cost
           (get-in graph [curr first-person] 0)
           (get-in graph [first-person curr] 0))))))

(parse-graph "day13-example.txt")
(defn answer-part1 [filename]
  (let [graph (parse-graph filename)]
    (->> (keys graph)
         (combo/permutations)
         (map (partial ordering-cost graph))
         (sort)
         (last))))

(answer-part1 "day13-example.txt")
(answer-part1 "day13.txt")

(defn answer-part2 [filename]
  (let [graph (assoc (parse-graph filename) "You" {})]
    (->> (keys graph)
         (combo/permutations)
         (map (partial ordering-cost graph))
         (sort)
         (last))))

(answer-part2 "day13-example.txt")
(answer-part2 "day13.txt")
