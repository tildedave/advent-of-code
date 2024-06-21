(ns advent2019.day14
  (:require [utils :as utils]
            [graph :as graph]
            [clojure.set :as set]))

;; I wonder if I could use a back-substitution approach

;; awful but whatever.
(defn parse-line [^String line]
  (let [[left right] (.split line " => ")]
    [(apply hash-map (reverse (map utils/try-parse-int (.split right " "))))
     (into {} (map #(vec (reverse (map utils/try-parse-int (.split % " ")))) (.split left ", ")))]))

;; so I guess I did this before. (day14.go)
;; topo sort chemicals in order (does this matter?  yes it seems to)
;; always take from an earlier chemical if you can.


(defn to-graph [parsed-lines]
  (->> parsed-lines
       (map (fn [[result deps]]
         {(first (keys result)) (set (keys deps))}))
       (reduce merge {})))



(->> (utils/read-input "2019/day14-example.txt")
     (map parse-line)
     (to-graph)
     (graph/topological-sort))
