(ns advent2016.day24
  (:require [utils :as utils]
            [grid :as grid]
            [clojure.math.combinatorics :as combo]))

;; OK, so I think we can get all pairs shortest paths distances
;; for the numbered states and then just check all permutations
;; of the numbers (only 7! possibilities)
;; I think I can just repurpose my dijkstra code

(int \0)
(int \7)

(defn interesting-coords [grid]
  (->> (grid/coords grid)
       (filter #(<= (int \0)
                    (int (grid/at grid %))
                    (int \9)))
       (set)))

(defn adjacency-matrix [grid]
  (let [numbered-coords (interesting-coords grid)]
   (->>
    (for [coord numbered-coords]
      {coord (->> (grid/dijkstra-search
                   coord
                   #(grid/neighbors grid % grid/cardinal-directions (fn [ch] (= ch \#)))
                   (fn [_ _] false))
                  (filter (fn [[coord dist]] (and
                                              (> dist 0)
                                              (contains? numbered-coords coord))))
                  (into {})
                  )})
    (reduce merge))))

(defn path-length [adj path]
  (reduce + (map (fn [x y]
         (get-in adj [x y]))
       path (rest path))))

(defn answer [filename]
  (let [grid (grid/parse (utils/read-input filename))
        adj (adjacency-matrix grid)
        zero-coord (->> (grid/coords grid)
                        (filter #(= (grid/at grid %) \0))
                        (first))]
    (->> (combo/permutations (keys adj))
         (filter (fn [l] (= (first l) zero-coord)))
         (map (partial path-length adj))
         (sort)
         (first))))

(answer "2016/day24-example.txt")
(answer "2016/day24.txt")

(defn answer-part2 [filename]
  (let [grid (grid/parse (utils/read-input filename))
         adj (adjacency-matrix grid)
         zero-coord (->> (grid/coords grid)
                         (filter #(= (grid/at grid %) \0))
                         (first))]
     (->> (combo/permutations (keys adj))
          (map #(concat % (list zero-coord)) )
          (filter (fn [l] (= (first l) zero-coord)))
          (map (partial path-length adj))
          (sort)
          (first))))

(answer-part2 "2016/day24.txt")
)

  (combo/)(keys adj))



(grid/neighbors

 [1 1]
 grid/cardinal-directions
 (fn [ch] (not= ch \#)))
