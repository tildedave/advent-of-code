(ns advent2015.day9
  (:require [clojure.math.combinatorics :as combo]
            [utils :as utils]))

;; Shortest hamiltonian path is NP hard and 9 nodes is brute forceable.

(defn parse-line [line]
  (let [[_ src dest cost-str] (re-matches #"(\w+) to (\w+) = (\d+)" line)
        cost (utils/parse-int cost-str)]
    {src [dest cost] dest [src cost]}))

(defn path-cost [costs path]
  (loop
   [curr (first path)
    total-cost 0
    path (rest path)]
    (if-let [next (first path)]
      (if-let [cost (get-in costs [curr next])]
        (recur next (+ total-cost cost) (rest path))
        -1)
      total-cost)))

(defn brute-force-path-lengths [costs cities]
  (->> (combo/permutations cities)
       (map (partial path-cost costs))
       (remove (partial = -1))
       (sort)))

(defn answer-part1 [filename]
  (let [costs (->> (utils/read-input (format "2015/%s" filename))
                   (map parse-line)
                   (reduce (partial merge-with concat))
                   (#(update-vals % (fn [v] (apply hash-map v)))))
        cities (keys costs)]
    (first (brute-force-path-lengths costs cities))))

(answer-part1 "day9.txt")


(defn answer-part2 [filename]
  (let [costs (->> (utils/read-input (format "2015/%s" filename))
                    (map parse-line)
                    (reduce (partial merge-with concat))
                    (#(update-vals % (fn [v] (apply hash-map v)))))
         cities (keys costs)]
     (last (brute-force-path-lengths costs cities))))


(answer-part2 "day9.txt")

