(ns graph
  (:require [clojure.set :as set]))

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
