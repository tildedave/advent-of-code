(ns graph
  (:require [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]))

;; we search for the end.
;; cost to get to the end is just our risk.
;; I am going to extract this to a common function because I'm tired of
;; programming A* search :-)
(defn a*-search
  ([start is-goal? neighbors heuristic distance]
   (a*-search start is-goal? neighbors heuristic distance identity))
  ([start is-goal? neighbors heuristic distance state-hash]
   (loop [[goal-score open-set nodes]
          [{(state-hash start) 0}
           (priority-map start 0)
           0]]
    ;; (println (peek open-set))
     (if-let [[current _] (peek open-set)]
       (cond
         (is-goal? current) [(goal-score (state-hash current)) current]
         (> nodes 1000000) (throw (Exception. "too many nodes"))
         :else (recur
                (reduce
                 (fn [[goal-score open-set nodes] neighbor]
                   (let [n-distance (distance current neighbor)
                         n-new-score (+ (goal-score (state-hash current)) n-distance)
                         n-score (get goal-score (state-hash neighbor) Integer/MAX_VALUE)]
                     (if (< n-new-score n-score)
                       [(assoc goal-score (state-hash neighbor) n-new-score)
                        (assoc open-set neighbor (+ n-new-score (heuristic neighbor)))
                        nodes]
                       [goal-score open-set nodes])))
                 [goal-score (pop open-set) (inc nodes)]
                 (neighbors current))))
       (throw (Exception. "could not reach goal"))))))

(defn dijkstra-search [start neighbors should-cutoff?]
  (loop [[visited distance queue] [#{} {start 0} (priority-map start 0)]]
    (if (empty? queue)
      distance
      (let [[current dist] (peek queue)
            queue (pop queue)
            visited (conj visited current)]
        (if (should-cutoff? current dist)
          (recur [visited distance queue])
          (recur
           (reduce
            (fn [[visited distance queue] neighbor]
              (if
               (contains? visited neighbor) [visited distance queue]
               (let [alt (inc (distance current))]
                 (if (< alt (get distance neighbor Integer/MAX_VALUE))
                   [visited (assoc distance neighbor alt) (assoc queue neighbor alt)]
                   [visited distance queue]))))
            [visited distance queue]
            (neighbors current))))))))

(defn breadth-first-search
  ([start neighbors] (breadth-first-search start nil neighbors (fn [& args] false)))
  ([start end neighbors should-cutoff?]
   (loop [[visited distances parent queue] [#{start} {start 0} {} (priority-map start 0)]]
     (if (empty? queue)
       [parent distances nil]
       (let [[current dist] (peek queue)
             queue (pop queue)]
         (cond
           (= current end) [parent distances dist]
           (should-cutoff? current dist) (recur [visited distances parent queue])
           :else (recur
                  (reduce
                   (fn [[visited distances parent queue] neighbor]
                     (if (contains? visited neighbor)
                       [visited distances parent queue]
                       [(conj visited neighbor)
                        (assoc distances neighbor (inc dist))
                        (assoc parent neighbor current)
                        (assoc queue neighbor (inc dist))]))
                   [visited distances parent queue]
                   (neighbors current)))))))))

(peek [1 2 3 4])

;; idea from
;; https://stackoverflow.com/questions/14144071/finding-all-the-shortest-paths-between-two-nodes-in-unweighted-undirected-graph
(defn all-paths [source dest neighbors should-cutoff?]
  (let [[parent distances _] (breadth-first-search source dest neighbors should-cutoff?)]
    ;; we have a "current path", which generates multiple other paths
    (letfn [(paths-back [path-so-far]
              (let [x (peek path-so-far)]
                (if (= x source)
                  [(rseq path-so-far)]
                  (let [x-dist (distances x)
                        is-valid-neighbor? #(= (distances %) (dec x-dist))
                        is-source-adjacent? (= (parent x) source)]
                              ;; this is a bit of a hack since the "neighbors"
                              ;; function might not allow stepping back into
                              ;; source due to source being in the grid.
                    (if is-source-adjacent?
                      [(rseq (conj path-so-far source))]
                      (->> (neighbors x)
                           (filter is-valid-neighbor?)
                           (map (partial conj path-so-far))
                           (map paths-back)
                           (apply concat)))))))]
      (paths-back [dest]))))

;; queue is going to be the resulting order
;; graph is a map from a node to its dependencies.
;; e.g. Node -> {things that depend on node}
(defn topological-sort [graph]
  (let [all-nodes (set/union (reduce set/union (vals graph)) (set (keys graph)))
        deps (->> all-nodes
                  (map #(map (fn [n] {n #{%}}) (graph %)))
                  (map #(reduce merge {} %))
                  (reduce (partial merge-with set/union) {}))]
  (loop [queue (->> all-nodes (remove deps) (vec))
         deps deps
         result []]
    (if-let [x (first queue)]
      (let [queue (subvec queue 1)
            [queue deps]
            (reduce
             (fn [[queue deps] curr]
               (let [deps (update deps curr #(disj % x))
                     queue (if (empty? (deps curr))
                             (conj queue curr)
                             queue)]
                 [queue deps]))
             [queue deps]
             (graph x))]
        (recur queue deps (conj result x)))
        result))))
