(ns advent2019.day6
  (:require [grid :as grid]
            [utils :as utils]))

;; I suppose this is easiest to do using DFS.

(defn parse-orbit [^String line]
  (apply hash-map (reverse (.split line "\\)"))))

(defn parse-orbits [lines]
  (reduce merge {} (map parse-orbit lines)))

(parse-orbits (utils/read-input "2019/day6-example.txt"))

(defn answer-part1 [lines]
  (let [orbits (parse-orbits lines)]
    (->> (keys orbits)
         (map #(-> (grid/breadth-first-search
                    %
                    (fn [n]
                      (if (contains? orbits n)
                        [(orbits n)]
                        [])))
                   (first)
                   (keys)
                   (set)
                   (disj %)))
         (map count)
         (reduce +))))

(answer-part1 (utils/read-input "2019/day6-example.txt"))
(answer-part1 (utils/read-input "2019/day6.txt"))
