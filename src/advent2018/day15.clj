(ns advent2018.day15
  (:require [grid :as grid]
            [utils :as utils]))

;; this problem is essentially iterated dijsktra
;; dijkstra needs to understand when there are multiple shortest paths.
;; I suppose we could just DFS/BFS from the start to the intended
;; destination and try to get all paths.

(defn agents [grid]
  (->> (grid/coords grid)
       (filter #(contains? #{\E \G} (grid/at grid %)))
       (map #(let [id (random-uuid)] {id {:type (grid/at grid %) :coords % :id id}}))
       (reduce merge {})))

(agents (grid/parse (utils/read-input "2018/day15-move-example.txt")))

(defn reading-order-compare [[x1 y1] [x2 y2]]
  (let [y-comp (compare y1 y2)
        x-comp (compare x1 x2)]
    (case y-comp
      0 x-comp
      -1 y-comp
      1 y-comp)))

(defn neighbors [grid]
  #(grid/neighbors grid % grid/cardinal-directions (fn [ch] (not (= \. ch)))))

(defn next-target [grid neighbors agents current-agent-id]
    (let [current-agent (agents current-agent-id)
          agent-coords (:coords current-agent)
          agent-type (:type current-agent)
          other-type (case agent-type \E \G \G \E)
          distances (grid/dijkstra-search
                     agent-coords
                     neighbors
                     (fn [& _] false))
          target-coords (->> (vals agents)
                       (filter #(= other-type (:type %)))
                       (map :coords))
          nearby-targets (reduce concat [] (map neighbors target-coords))
          target (->> nearby-targets
                      (filter distances)
                      (sort (fn [c1 c2]
                              (let [distance-comp (compare (distances c1) (distances c2))]
                                (if
                                 (= distance-comp 0) (reading-order-compare c1 c2)
                                 distance-comp))))
                      (first))]
      [target (distances target)]))

(let [grid (grid/parse (utils/read-input "2018/day15-move-example.txt"))]
  (grid/breadth-first-search
   [1 1]
   [3 1]
   (neighbors grid)
   (fn [& _] false)
   ))

(let [grid (grid/parse (utils/read-input "2018/day15-move-example.txt"))]
  (grid/all-paths [1 1] [2 2] (neighbors grid) (fn [& args] false)))

(defn agent-move [grid neighbors agents current-agent-id]
  (let [_ (println current-agent-id)
        current-square (get-in agents [current-agent-id :coords])
        [target target-dist] (next-target grid neighbors agents current-agent-id)]
    (assoc-in
     agents
     [current-agent-id :coords]
     (if (= target current-square)
       current-square
       (->> (grid/all-paths (get-in agents [current-agent-id :coords]) target neighbors
                            (fn [_ d] (> d target-dist)))
            (sort-by first reading-order-compare)
            ;; first path is the "reading order first"
            (first)
            ;; second element of it is the next step
            (second))))))

(defn agent-id-order [agents]
  (->> (vals agents)
       (sort-by :coords reading-order-compare)
       (map :id)))

(defn tick [grid]
  (let [neighbors (memoize (neighbors grid))]
    (fn [agents]
      (reduce
       (partial agent-move grid neighbors)
       agents
       (agent-id-order agents)))))

(let [grid (grid/parse (utils/read-input "2018/day15-move-example2.txt"))]
  (-> (iterate (tick grid) (agents grid))))

(let [grid (grid/parse (utils/read-input "2018/day15-move-example.txt"))
          agents (agents grid)
          neighbors (memoize (neighbors grid))
          _ (println "agents are "  agents)
          elf-agent-id (->> (vals agents) (filter #(= (:coords %) [1 1])) (first) (:id))]
      (agent-move grid neighbors agents elf-agent-id)))(agents grid)
  ((agent-move grid) (agent-coords grid) [1 1]))


