(ns advent2022.day16
  (:require [advent2022.utils :as utils]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]))

;; I guess we start with floyd warshall, eliminate the 00 flow nodes, and
;; perform some kind of search.

(def lines (utils/read-resource-lines "input/day16-example.txt"))

(def valve-re #"Valve (\w+) has flow rate=(\d+); (tunnels lead to valves|tunnel leads to valve) (\w+(, \w+)*)")

(re-matches valve-re "Valve HH has flow rate=22; tunnel leads to valve GG")

(defn parse-line [[adjacency flow] line]
  (let [[_ valve flow-str _ tunnels-str] (re-matches valve-re line)
        valve-flow (utils/parse-int flow-str)
        tunnels (.split tunnels-str ", ")]
    [(assoc adjacency valve (set tunnels))
     (assoc flow valve valve-flow)]))


;; so now we floyd-warshall the graph, just so we don't waste any time faffing
;; about on intermediate nodes.

(def adjacency-matrix (first (reduce parse-line [{} {}] lines)))
(def flows (second (reduce parse-line [{} {}] lines)))

(defn adjacency-edges [adjancency]
  (reduce
   (fn [acc k]
     (reduce
      (fn [acc v]
        (conj acc [k v]))
      acc
      (adjancency k)))
   []
   (keys adjancency)))
(adjacency-edges adjacency-matrix)

(defn all-pairs-shortest-paths [adjacency]
  (let [initial-distances (->> (keys adjacency)
                               (into {} (map #(vector [% %] 0)))
                               (merge (into {} (map #(vector % 1) (adjacency-edges adjacency)))))]
    (reduce
     (fn [distances [k i j]]
       (let [dist-i-j (get distances [i j] Integer/MAX_VALUE)
             dist-k-j (get distances [k j] Integer/MAX_VALUE)
             dist-i-k (get distances [i k] Integer/MAX_VALUE)]
         (if (> dist-i-j (+ dist-i-k dist-k-j))
           (assoc distances [i j] (+ dist-i-k dist-k-j))
           distances)))
     initial-distances
     (for [k (keys adjacency)
           i (keys adjacency)
           j (keys adjacency)]
       [k i j]))))


;; following a reddit comment we'll use A* search and use the cost function
;; as any valve NOT open causes the cost to increase.
;; we don't have a goal.  our final nodes will all have timeLeft < 0 and we'll
;; take the min cost from that.
;; we similarly don't really have a heuristic function.
;; I guess A* is just fancy Dijkstra's in this case.

(map-indexed vector [1 2])

(def valves
  (->> flows
       (filter #(> (second %) 0))
       (map first)
       (apply hash-set)))

(def bit-idx (into {} (map-indexed vector valves)))
(def reverse-bit-idx (set/map-invert bit-idx))

(defn calculate-cost [open-valves elapsed-time]
  (->> valves
       (remove #(bit-test open-valves (reverse-bit-idx %)))
       (map flows)
       (reduce + 0)
       (* elapsed-time)))

(defn search []
  (let [start ["AA" 0 30]
        distances (all-pairs-shortest-paths adjacency-matrix)]
    (loop [[open-set goal-score came-from] [(priority-map start 0)  ;; open-set has fscore
                                            (assoc {} start 0) ;; goal-score is gscore
                                            {}]]
      (if (empty? open-set) [goal-score came-from]
          (let [[current-node] (peek open-set)
                [location open-valves time-left] current-node
                open-set (pop open-set)
                next-locations (->> valves
                                    (remove #(bit-test open-valves (reverse-bit-idx %))))
                ;; add "standing pat" as a valid location, and try to understand its cost.
                ;; we DON'T have to add it to the queue.
                stand-pat-node [location open-valves 0]
                stand-pat-score (+ (goal-score current-node) (calculate-cost open-valves time-left))
                should-stand-pat (< stand-pat-score (get goal-score stand-pat-node Integer/MAX_VALUE))
                goal-score (if should-stand-pat
                             (assoc goal-score stand-pat-node stand-pat-score)
                             goal-score)
                came-from (if should-stand-pat
                            (assoc came-from stand-pat-node current-node)
                            came-from)]
            ;; we want to end up calling recur() on this
            ;; I sort of hate this setup, it seems like it forces me to
            ;; make my loop variables destructured so the recur doesn't need to
            ;; un-bind/re-bind.
            ;; (println "I am at" current-node "score is" (goal-score current-node))
            ;; (println "stand pat node" stand-pat-node "with score" stand-pat-score)
            (recur
             (reduce
              (fn [[open-set goal-score came-from] next-location]
                ;; we need to convert our next-location into a full node in the
                ;; graph which includes the open valves and time left.
                ;; also need to calculate the cost.
                (let [next-open-valves (bit-set open-valves (reverse-bit-idx next-location))
                      ;; inc because we open the valve at the next location.
                      elapsed-time (inc (distances [location next-location]))
                      ;; cost = distance * every closed valve * flow value of every closed valve
                      ;; we also haven't opened the valve yet so the cost includes the valve
                      ;; we're about to open.
                      cost (calculate-cost open-valves elapsed-time)
                      tentative-gscore (+ (goal-score current-node) cost)
                      next-time-left (- time-left elapsed-time)
                      next-node [next-location next-open-valves next-time-left]]
                  (if (and
                       (>= next-time-left 0)
                       (< tentative-gscore (get goal-score next-node Integer/MAX_VALUE)))
                    (do
                      ;; (println "going to node" next-node "with cost" cost "(takes" elapsed-time "minutes)")
                      ;; (println "setting goal-score" next-node "to" tentative-gscore)
                    ;; so we add it here
                      [(assoc open-set next-node (+ tentative-gscore (- 30 (flows next-location))))
                       (assoc goal-score next-node tentative-gscore)
                       (assoc came-from next-node current-node)])
                    ;; otherwise we don't bother
                    (do
                      ;; (println "no reason to go to" next-node "- next node score" (get goal-score next-node))
                    [open-set goal-score came-from]))))
              [open-set goal-score came-from]
              next-locations)))))))

(def distances (all-pairs-shortest-paths adjacency-matrix))

(defn reconstruct-path [node came-from]
  (loop [curr node result []]
    (if (contains? came-from curr)
      (recur (came-from curr) (conj result curr))
      (reverse (map first (conj result curr))))))

(- (calculate-cost 0 30) 779)

(let [[goal-score came-from] (search)
      distances (all-pairs-shortest-paths adjacency-matrix)
      true-goal-scores (->> goal-score
                            (filter (fn [[[_ _ time-left]]] (= time-left 0))))
      best-node (first (sort-by second true-goal-scores))
      worst-score (calculate-cost 0 30)]
  (- worst-score (second best-node)))

      path (drop-last (reconstruct-path (first best-node) came-from))]
  (println best-node)
  (println path)
  (score path distances flows))

;; correct answer is AA DD BB JJ HH EE CC
;; for whatever reason, we choose the wrong one.
;; also not clear why we don't include EE / CC in the final.
;; 1522 cost so far.
