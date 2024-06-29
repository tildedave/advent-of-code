(ns advent2019.day20
  (:require [grid :as grid]
            [utils :as utils]
            [clojure.math.combinatorics :as combo]))

;; so grid bounds don't work but whatever.



(defn is-portal? [grid coord]
  (if-let [x (grid/at grid coord)]
    (Character/isAlphabetic (int x))
    false))

(defn is-adjacent? [coord1 coord2]
  (= (utils/manhattan-distance coord1 coord2) 1))

(defn portal-coord [grid coord1 coord2]
  ;; if the portals are vertically aligned, it's the one either above or below it.
  ;; if the portals are horizontally aligned, it's the one either left or right.
  (let [delta (mapv - coord1 coord2)]
    (->>
     (list
     (mapv + coord1 delta)
     (mapv + coord2 delta)
     (mapv + coord1 (mapv - delta))
     (mapv + coord2 (mapv - delta)))
     (filter #(= \. (grid/at grid %)))
     (first))))

(defn portals [grid]
  (let [alpha-coords (filter (partial is-portal? grid) (grid/coords grid))
        _ (println "alpha coords" alpha-coords)]
    (->> (combo/combinations alpha-coords 2)
         (filter #(apply is-adjacent? %))
         (map (fn [[c1 c2]] (vec (sort [c1 c2]))))
         (map (fn [[c1 c2]]
                {[(grid/at grid c1) (grid/at grid c2)] [(portal-coord grid c1 c2)]}
                ))
         (reduce (partial merge-with concat) {})
         )))

(portals (grid/parse-file "2019/day20-example.txt"))
