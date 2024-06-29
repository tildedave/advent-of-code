(ns advent2019.day20
  (:require [grid :as grid]
            [graph :as graph]
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
  (let [alpha-coords (filter (partial is-portal? grid) (grid/coords grid))]
    (->> (combo/combinations alpha-coords 2)
         (filter #(apply is-adjacent? %))
         (map (fn [[c1 c2]] (vec (sort [c1 c2]))))
         (map (fn [[c1 c2]]
                {[(grid/at grid c1) (grid/at grid c2)] [(portal-coord grid c1 c2)]}
                ))
         (reduce (partial merge-with concat) {})
         )))

(defn reverse-portals [portals]
  (reduce merge (for [[k vlist] portals]
    (reduce merge
            (for [v vlist]
      {v k})))))


(portals (grid/parse-file "2019/day20-example.txt"))
(reverse-portals (portals (grid/parse-file "2019/day20-example.txt")))


;; so part 1 is just dijkstra search
;; part 2 is also just dijkstra search
;; our dijkstra search doesn't stop at the end so we'll use our A* search,
;; which does.

(defn minimum-path [grid]
  (let [portals (portals grid)
        reverse-portals (reverse-portals portals)
        start (first (portals [\A \A]))
        end (first (portals [\Z \Z]))]
    (graph/a*-search
     start
     (partial = end)
     (fn [current]
       ;; neighbors are our neighbors as per normal grid, and, if we're in a
       ;; portal, then the other node
       (let [walking-neighbors (grid/neighbors grid current grid/cardinal-directions (fn [ch] (not= ch \.)))]
         (if (and (contains? reverse-portals current) (not= start current))
           (let [warp-neighbor (first (filter (partial not= current) (portals (reverse-portals current))))]
             (cons warp-neighbor walking-neighbors))
           walking-neighbors)))
     (fn [& _] 0) ;; no heuristic
     (fn [& _] 1) ;; everything is 1 step
     )))

;; part 1
(minimum-path (grid/parse-file "2019/day20.txt"))
