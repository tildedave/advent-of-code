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

(defn maybe-add-score [node current-node score goal-score came-from]
  (let [should-add (< score (get goal-score node Integer/MAX_VALUE))]
    [(if should-add (assoc goal-score node score) goal-score)
     (if should-add (assoc came-from node current-node) came-from)
     should-add]))

(defn my-test [[a b c :as all] d]
  {:a a :b b :c c :d d :all all})

(my-test [1 2 3 4 5 6 7] 8)

(defn is-valve-open? [open-valves valve]
  (bit-test open-valves (reverse-bit-idx valve)))

(defn open-valve [open-valves valve]
  (bit-set open-valves (reverse-bit-idx valve)))

(defn process-neighbor [current-node distances]
  (let [[location open-valves time-left] current-node]
    (fn [[open-set goal-score came-from] next-location]
                ;; we need to convert our next-location into a full node in the
                ;; graph which includes the open valves and time left.
                ;; also need to calculate the cost.
      (let [next-open-valves (open-valve open-valves next-location)
                      ;; inc because we open the valve at the next location.
            elapsed-time (inc (distances [location next-location]))
                      ;; cost = distance * every closed valve * flow value of every closed valve
                      ;; we also haven't opened the valve yet so the cost includes the valve
                      ;; we're about to open.
            next-time-left (- time-left elapsed-time)
            tentative-gscore (if (< next-time-left 0) Integer/MAX_VALUE
                                 (+ (goal-score current-node) (calculate-cost open-valves elapsed-time)))
            next-node [next-location next-open-valves next-time-left]
            [goal-score came-from added] (maybe-add-score next-node current-node tentative-gscore goal-score came-from)]
        (if added
                      ;; (println "going to node" next-node "with cost" cost "(takes" elapsed-time "minutes)")
                      ;; (println "setting goal-score" next-node "to" tentative-gscore)
                    ;; so we add it here
          [(assoc open-set next-node (+ tentative-gscore (- 30 (flows next-location))))
           goal-score
           came-from]
                    ;; otherwise we don't bother
                      ;; (println "no reason to go to" next-node "- next node score" (get goal-score next-node))
          [open-set goal-score came-from])))))


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
                                    (remove (partial is-valve-open? open-valves)))
                ;; add "standing pat" as a valid location, and try to understand its cost.
                ;; we DON'T have to add it to the queue.
                stand-pat-node [location open-valves 0]
                stand-pat-score (+ (goal-score current-node) (calculate-cost open-valves time-left))
                [goal-score came-from] (maybe-add-score stand-pat-node current-node stand-pat-score goal-score came-from)]
            ;; we want to end up calling recur() on this
            ;; I sort of hate this setup, it seems like it forces me to
            ;; make my loop variables destructured so the recur doesn't need to
            ;; un-bind/re-bind.
            ;; (println "I am at" current-node "score is" (goal-score current-node))
            ;; (println "stand pat node" stand-pat-node "with score" stand-pat-score)
            (recur
             (reduce
              (process-neighbor current-node distances)
              [open-set goal-score came-from]
              next-locations)))))))

(def distances (all-pairs-shortest-paths adjacency-matrix))

(defn reconstruct-path [node came-from]
  (loop [curr node result []]
    (if (contains? came-from curr)
      (recur (came-from curr) (conj result curr))
      (reverse (map first (conj result curr))))))

(let [[goal-score] (search)
      true-goal-scores (->> goal-score
                            (filter (fn [[[_ _ time-left]]] (= time-left 0))))
      best-node (first (sort-by second true-goal-scores))
      worst-score (calculate-cost 0 30)]
  (- worst-score (second best-node)))

;; so this works for part 1
;; for part 2, we have an elephant, and less time.
;; kind of feels like floyd-warshall isn't helpful, though it does increase
;; the search state.  the elephant and you moving at once seems important,
;; and if it takes you + elephant different time to get there, things are
;; going to get rough in the logic.
;; so OK, we'll rewrite the algorithm to have the elephant, won't try to share
;; code, at least at first.  but scale isn't much worse and our approach
;; was shown to work for part 1.

(peek (priority-map "abc" 1))

;; actually we can make this usable for both me and elephant (bffs).
;; the real elephant combo logic will be in list merging.
(defn get-neighbor-node-list [[location open-valves time-left]]
  (if (and (> (get flows location 0) 0)
           (not (is-valve-open? open-valves location)))
    [[location (open-valve open-valves location) (dec time-left)]]
    ;; otherwise process each neighbor of the adjacency matrix
    (reduce
     (fn [acc neighbor-location]
       (conj acc [neighbor-location open-valves (dec time-left)]))
     []
     (adjacency-matrix location))))

(adjacency-matrix "DD")

(get-neighbor-node-list ["DD" 4 28])

(and (> (get flows "DD" 0) 0)
     (not (is-valve-open? 0 "DD")))

(> (get flows "DD" 0) 0)

(defn combo-lists [list1 list2]
  (remove
   nil?
   (for [[location open-valves time-left] list1
         [elephant-location ele-open-valves time-left] list2]
     (if (= location elephant-location) nil
         [location elephant-location (bit-or open-valves ele-open-valves) time-left]))))

(adjacency-matrix "AA")

(combo-lists
 (get-neighbor-node-list ["AA" 0 26])
 (get-neighbor-node-list ["AA" 0 26]))

(use 'clojure.tools.trace)

(defn search-with-elephant []
  (let [start ["AA" "AA" 0 30]]
    (loop [[open-set goal-score came-from] [(priority-map start 0) (assoc {} start 0) {}]
           i 0]
      (if
       (empty? open-set) [goal-score came-from]
       (let [[current-node] (peek open-set)
             [location ele-location open-valves time-left] current-node
             open-set (pop open-set)
            ;; we need to process both my action and the elephant's action
            ;; IF I open a valve, we move on to the elephant.
            ;; if there's no valve worth opening we generate a list of next
            ;; positions and need to glom the elephants on.
            ;; so it's like, my-next and elephant-next, these are lists, then
            ;; there's some method of merging the lists.
            ;; oh yes, standing pat should also be added.
             stand-pat-node [location ele-location open-valves 0]
             stand-pat-score (+ (goal-score current-node) (calculate-cost open-valves time-left))
            ;; fun, the logic is basically the same.
             [goal-score came-from] (maybe-add-score stand-pat-node current-node stand-pat-score goal-score came-from)]
         (println "current node" current-node)
        ;;  (println "my neighbors" (get-neighbor-node-list [location open-valves time-left]))
        ;;  (println "elephant neighbors" (get-neighbor-node-list [ele-location open-valves time-left]))
          (recur
           (reduce
            (fn
              [[open-set goal-score came-from] next-node]
              (let [[_ _ open-valves time-left] next-node
                    tentative-gscore (if (< time-left 0) Integer/MAX_VALUE
                                         (+ (goal-score current-node) (calculate-cost open-valves 1)))
                    [goal-score came-from added] (maybe-add-score next-node current-node tentative-gscore goal-score came-from)
                    heuristic-fudge (- 30 (flows (first next-node)) (flows (second next-node)))]
                [(if added
                   (assoc open-set next-node (+ tentative-gscore heuristic-fudge))
                   open-set)
                 goal-score
                 came-from]))
            [open-set goal-score came-from]
            (combo-lists
             (get-neighbor-node-list [location open-valves time-left])
             [["AA" open-valves (dec time-left)]]))
            ;;  (get-neighbor-node-list [ele-location open-valves time-left])))
           (inc i)))))))

;; so this is implemented but it does not work.
;; I suppose an easy way to check this is if the elephant doesn't move and we
;; start at 30 minutes left.
(let [[goal-score] (search-with-elephant)
      true-goal-scores (->> goal-score
                            (filter (fn [[[_ _ _ time-left]]] (= time-left 0))))
      best-node (first (sort-by second true-goal-scores))
      worst-score (calculate-cost 0 30)]
  (- worst-score (second best-node)))
