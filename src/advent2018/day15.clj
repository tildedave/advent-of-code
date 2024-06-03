(ns advent2018.day15
  (:require [grid :as grid]
            [utils :as utils]))

;; this problem is essentially iterated dijsktra
;; dijkstra needs to understand when there are multiple shortest paths.
;; I suppose we could just DFS/BFS from the start to the intended
;; destination and try to get all paths.

(defn agent-coords [grid]
  (->> (grid/coords grid)
       (filter #(contains? #{\E \G} (grid/at grid %)))
       (set)))

(agent-coords (grid/parse (utils/read-input "2018/day15-move-example.txt")))

(defn reading-order-compare [[x1 y1] [x2 y2]]
  (let [y-comp (compare y1 y2)
        x-comp (compare x1 x2)]
    (case y-comp
      0 x-comp
      -1 y-comp
      1 y-comp)))

(defn neighbors [grid]
  #(grid/neighbors grid % grid/cardinal-directions (fn [ch] (not (= \. ch)))))

(defn next-target [grid neighbors agents current-agent]
    (let [agent-coords (:coords current-agent)
          distances (grid/dijkstra-search
                     agent-coords
                     neighbors
                     (fn [& _] false))
          agent-type (grid/at grid agent-coords)
          other-type (case agent-type \E \G \G \E)
          targets (->> agents
                       (filter #(= other-type (grid/at grid %))))
          nearby-targets (reduce concat [] (map neighbors targets))]
      (->> nearby-targets
           (filter distances)
           (sort (fn [c1 c2]
                   (let [distance-comp (compare (distances c1) (distances c2))]
                     (if
                      (= distance-comp 0) (reading-order-compare c1 c2)
                      distance-comp)))))))
(let [grid (grid/parse (utils/read-input "2018/day15-move-example.txt"))]
  (grid/breadth-first-search
   [1 1]
   [3 1]
   (neighbors grid)
   (fn [& _] false)
   ))

(let [grid (grid/parse (utils/read-input "2018/day15-move-example.txt"))]
  (grid/all-paths [1 1] [2 2] (neighbors grid) (fn [& args] false)))

(defn agent-move [grid neighbors agents current-agent]
  (let [target (next-target grid neighbors agents current-agent)]
    (->> (grid/all-paths (:coords current-agent) target neighbors (fn [& args] false))
         (sort-by first reading-order-compare)
         (first))))

(let [grid (grid/parse (utils/read-input "2018/day15-move-example.txt"))]
  ((agent-move grid) (agent-coords grid) [1 1]))


