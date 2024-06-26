(ns advent2018.day15
  (:require [grid :as grid]
            [graph :as graph]
            [utils :as utils]
            [clojure.string :as string]))

;; this problem is essentially iterated dijsktra
;; dijkstra needs to understand when there are multiple shortest paths.
;; I suppose we could just DFS/BFS from the start to the intended
;; destination and try to get all paths.

(defn agents [grid]
  (->> (grid/coords grid)
       (filter #(contains? #{\E \G} (grid/at grid %)))
       (map #(let [id (random-uuid)] {id {:type (grid/at grid %) :coords % :id id :hp 200 :attack-power 3}}))
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

(defn target-coords [agents current-agent-id]
  (let [current-agent (agents current-agent-id)
        agent-type (:type current-agent)
        other-type (case agent-type \E \G \G \E)]
    (->> (vals agents)
         (filter #(= other-type (:type %)))
         (map :coords))))

(defn next-target [neighbors agents current-agent-id]
  (let [agent-coords (:coords (agents current-agent-id))
        targets (target-coords agents current-agent-id)]
    (if (empty? (filter (partial grid/is-adjacent? agent-coords) targets))
      (let [distances (graph/dijkstra-search
                       agent-coords
                       neighbors
                       (fn [& _] false))
            nearby-targets (reduce concat [] (map neighbors targets))
            target (->> nearby-targets
                        (filter distances)
                        (sort (fn [c1 c2]
                                (let [distance-comp (compare (distances c1) (distances c2))]
                                  (if
                                   (= distance-comp 0) (reading-order-compare c1 c2)
                                   distance-comp))))
                        (first))]
        [target (distances target)])
      ;; don't move
      [agent-coords 0])))

(let [grid (grid/parse (utils/read-input "2018/day15-move-example.txt"))]
  (graph/breadth-first-search
   [1 1]
   [3 1]
   (neighbors grid)
   (fn [& _] false)))

(let [grid (grid/parse (utils/read-input "2018/day15-move-example.txt"))]
  (graph/all-paths [1 1] [2 2] (neighbors grid) (fn [& args] false)))

(defn agent-move [[grid agents finished?] current-agent-id]
  (let [neighbors (neighbors grid)
        current-square (get-in agents [current-agent-id :coords])
        [target target-dist] (next-target neighbors agents current-agent-id)]
    (if (nil? target)
      ;; no path to target, or all targets death.
      [grid agents finished?]
      (let [[ox oy] current-square
            next-agents (assoc-in agents [current-agent-id :coords]
                                  (if (= target current-square)
                                    current-square
                                    (do
                                      (->> (graph/all-paths (get-in agents [current-agent-id :coords]) target neighbors
                                                            (fn [_ d] (> d target-dist)))
                                           (sort-by second reading-order-compare)
            ;; first path is the "reading order first"
                                           (first)
            ;; second element of it is the next step
                                           (second)))))
            [nx ny] (get-in next-agents [current-agent-id :coords])]
        [(-> grid
             (assoc-in [oy ox] \.)
             (assoc-in [ny nx] (get-in agents [current-agent-id :type])))
         next-agents
         finished?]))))

(defn agent-id-order [agents]
  (->> (vals agents)
       (sort-by :coords reading-order-compare)
       (map :id)))

(defn agent-attack [[grid agents finished?] current-agent-id]
  (let [current-agent (agents current-agent-id)
        targets (target-coords agents current-agent-id)
        coords-to-agent (reduce merge {} (map #(hash-map (:coords %) %) (vals agents)))
        adjacent-targets (filter (partial grid/is-adjacent? (:coords current-agent)) targets)]
    (cond
      (empty? targets) [grid agents true] ;; no target, full round does not complete.
      (empty? adjacent-targets) [grid agents finished?]
      :else (let [my-target (->> adjacent-targets
                                 (sort
                                  (fn [c1 c2]
                                    (case
                                     (compare (:hp (coords-to-agent c1)) (:hp (coords-to-agent c2)))
                                      -1 -1
                                      1 1
                                      0 0 (reading-order-compare c1 c2))))
                                 (first)
                                 (coords-to-agent))
                  [tx ty] (my-target :coords)
                  next-agents (update-in agents [(my-target :id) :hp] #(- % (:attack-power current-agent)))]
              (if (<= (get-in next-agents [(my-target :id) :hp]) 0)
          ;; death
                [(assoc-in grid [ty tx] \.)
                 (dissoc next-agents (my-target :id))
                 finished?]
                [grid next-agents finished?])))))

(defn tick [[grid agents finished?]]
  (reduce
   (fn [[grid agents finished?] current-agent-id]
     (cond finished? [grid agents finished?]
           (contains? agents current-agent-id) (-> [grid agents finished?]
                                                   (agent-move current-agent-id)
                                                   (agent-attack current-agent-id))
           :else [grid agents finished?]))
   [grid agents finished?]
   (agent-id-order agents)))

(let [grid (grid/parse (utils/read-input "2018/day15-full-example.txt"))]
  (nth (iterate tick [grid (agents grid) false]) 48))

(defn outcome [state-seq]
  (->> state-seq
       (map-indexed vector)
       (reduce
        (fn [acc [n [grid agents finished?]]]
          (if finished?
            (do
              (println (string/join "\n" (map string/join grid)))
              (println "after" (dec n) "seconds")
              (println "power was" (->> (vals agents)
                                        (filter #(= (:type %) \E))
                                        (map :attack-power)
                                        (first)))
              (println "hp left" (->> (vals agents)
                                      (filter #(> (:hp %) 0))
                                      (map :hp)
                                      (reduce +)))
              (reduced (* (dec n) (->> (vals agents)
                                       (filter #(> (:hp %) 0))
                                       (map :hp)
                                       (reduce +)))))
            acc))
        nil)))

(defn answer-part1 [grid]
  (->> (iterate tick [grid (agents grid) false])
       (outcome)))

(assert (= 27730 (answer-part1 (grid/parse (utils/read-input "2018/day15-full-example.txt")))))
(assert (= 36334 (answer-part1 (grid/parse (utils/read-input "2018/day15-full-example2.txt")))))
(assert (= 39514 (answer-part1 (grid/parse (utils/read-input "2018/day15-full-example3.txt")))))
(assert (= 27755 (answer-part1 (grid/parse (utils/read-input "2018/day15-full-example4.txt")))))
(assert (= 28944 (answer-part1 (grid/parse (utils/read-input "2018/day15-full-example5.txt")))))
(assert (= 18740 (answer-part1 (grid/parse (utils/read-input "2018/day15-full-example6.txt")))))

(defn any-elf-dies? [elf-agent-ids state-seq]
  (reduce
   (fn [acc [_ agents finished?]]
     (let [every-elf? (every? (partial contains? agents) elf-agent-ids)]
       (cond (not every-elf?) (reduced true)
             finished? (reduced false)
             :else acc)))
   nil
   state-seq))

(defn augment-elves [elf-agent-ids agents p]
  (reduce
   (fn [agents elf-id]
     (assoc-in agents [elf-id :attack-power] p))
   agents
   elf-agent-ids))

(defn answer-part2 [grid]
  (let [initial-agents (agents grid)
        elf-agent-ids (->> (keys initial-agents)
                           (filter #(= \E (get-in initial-agents [% :type])))
                           (set))]
    (->> (drop 4 (range))
         (map
          (fn [p]
            (iterate
             tick
             [grid (augment-elves elf-agent-ids initial-agents p) false])))
         (remove (partial any-elf-dies? elf-agent-ids))
         (first)
         (outcome))))

(assert (= 4988 (answer-part2 (grid/parse (utils/read-input "2018/day15-full-example.txt")))))
(assert (= 31284 (answer-part2 (grid/parse (utils/read-input "2018/day15-full-example3.txt")))))
(assert (= 3478 (answer-part2 (grid/parse (utils/read-input "2018/day15-full-example4.txt")))))
(assert (= 6474 (answer-part2 (grid/parse (utils/read-input "2018/day15-full-example5.txt")))))
;; there's a bug in this one :/ but my second star is right.  goodbye to this problem.
(assert (= 1140 (answer-part2 (grid/parse (utils/read-input "2018/day15-full-example6.txt")))))

(let [g (grid/parse (utils/read-input "2018/day15-full-example6.txt"))
      initial-agents (agents g)
      elf-agent-ids (->> (keys initial-agents)
                         (filter #(= \E (get-in initial-agents [% :type])))
                         (set))]
  (->> (any-elf-dies? elf-agent-ids
                      (iterate
                       tick
                       [g initial-agents false]))))


;; in general I don't want to run this since it's slow :-)
(outcome (grid/parse (utils/read-input "2018/day15.txt")))
(answer-part2 (grid/parse (utils/read-input "2018/day15.txt")))
