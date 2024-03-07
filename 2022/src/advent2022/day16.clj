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
                               (merge (into {} (map #(vector % 1) (adjacency-edges adjacency)))))
        initial-prev (->> (keys adjacency)
                          (into {} (map #(vector [% %] %)))
                          (merge (into {} (map (fn [[u v]] (vector [u v] u)) (adjacency-edges adjacency)))))]
    (reduce
     (fn [[distances prev] [k i j]]
       (let [dist-i-j (get distances [i j] Integer/MAX_VALUE)
             dist-k-j (get distances [k j] Integer/MAX_VALUE)
             dist-i-k (get distances [i k] Integer/MAX_VALUE)]
         (if (> dist-i-j (+ dist-i-k dist-k-j))
           [(assoc distances [i j] (+ dist-i-k dist-k-j))
            (assoc prev [i j] (prev [k j]))]
           [distances prev])))
     [initial-distances initial-prev]
     (for [k (keys adjacency)
           i (keys adjacency)
           j (keys adjacency)]
       [k i j]))))


(def prev (second (all-pairs-shortest-paths adjacency-matrix)))
(defn get-path [prev u v]
  (if
   (not (contains? prev [u v]))
    nil
    (loop [v v
           path '()]
      (if (not= u v)
        (recur (prev [u v]) (cons v path))
        (cons u path)))))

;; following a reddit comment we'll use A* search and use the cost function
;; as any valve NOT open causes the cost to increase.
;; we don't have a goal.  our final nodes will all have timeLeft < 0 and we'll
;; take the min cost from that.
;; we similarly don't really have a heuristic function, though we can fudge it.
;; I guess A* is just fancy Dijkstra's in this case.

(def valves
  (->> flows
       ;; filtering this makes some of our conditionals later a bit easier.
       ;;  (filter #(> (second %) 0))
       (map first)
       (apply hash-set)))

(def interesting-valves
  (filter #(> (flows %) 0) valves))

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

(defn is-valve-open? [open-valves valve]
  (bit-test open-valves (reverse-bit-idx valve)))

(defn open-valve [open-valves valve]
  (bit-set open-valves (reverse-bit-idx valve)))

(defn reconstruct-path [node came-from]
  (loop [curr node result []]
    (if (contains? came-from curr)
      (recur (came-from curr) (conj result curr))
      (reverse (map first (conj result curr))))))

;; we can make this usable for both me and elephant (bffs).
;; the real elephant combo logic will be in list merging.
(defn get-neighbors [[location path open-valves time-left]]
  ;; if we're on a path, we just push the path.
  (let [is-valve-closed (not (is-valve-open? open-valves location))
        has-flow (> (get flows location 0) 0)
        should-only-open-valve (>= (flows location) 4)]
    (cond
      ;; follow the path
      (seq path) [[(first path) (rest path) open-valves (dec time-left)]]
      ;; only open valves here
      (and is-valve-closed
           should-only-open-valve) [[location (list) (open-valve open-valves location) (dec time-left)]]
      :else
    ;; otherwise find any neighbor that we might want to go to.
    ;; we will filter out joint-destinations during the combo list phase.
        (reduce
         (fn [acc valve-to-open]
           (let [[next-location :as path] (rest (get-path prev location valve-to-open))]
             (conj acc [next-location (rest path) open-valves (dec time-left)])))
         (if (and has-flow is-valve-closed)
           [[location (list) (open-valve open-valves location) (dec time-left)]]
           [])
         ;; this needs to be all currently closed valves that we might ever
         ;; want to open
         (remove #(or (= % location) (is-valve-open? open-valves %)) interesting-valves)
         ))))

;; this seems to do what we want.
;; (last (take 32 (iterate (fn [x] (get-neighbors (first x))) [["AA" (list) 0 30]])))

(defn combo-lists [list1 list2]
  (remove
   nil?
   (for [[location path open-valves] list1
         [elephant-location ele-path ele-open-valves time-left] list2]
     (if
      (and (seq path)
           (seq ele-path)
           (= (last path) (last ele-path))) nil
      [location path elephant-location ele-path (bit-or open-valves ele-open-valves) time-left]))))

(defn process-neighbor [current-node]
  (fn [[open-set goal-score came-from] next-node]
    (let
     [[_ _ _ _ open-valves] current-node
      [loc path ele-loc ele-path next-open-valves time-left] next-node]
      (cond
      ;;  reduce search space by not returning to nodes that we
      ;;  already have a stand-pat score for w/same num open-valves.
        (contains? goal-score [loc () ele-loc () next-open-valves 0])
        [open-set goal-score came-from]
        ;; TODO: further pruning
        ;; if we have a better score opening MORE valves with MORE time, we don't
        ;; want to search this node.
        ;; this is more work per-node but it should pay off in pruning search space.
        ;; the challenge is in how to handle the paths.  maybe the pruning is OK.

        :else (let [tentative-gscore (if (< time-left 0) Integer/MAX_VALUE
                                   ;; must use our current open valves for cost.
                                         (+ (goal-score current-node) (calculate-cost open-valves 1)))
                    [goal-score came-from added] (maybe-add-score next-node current-node tentative-gscore goal-score came-from)
              ;; heuristic rewards changing the valve state.
              ;; can use heuristic for more goodness / reducing search state
                    heuristic (if (not= next-open-valves open-valves) 5 15)] ;(- 30 (flows (first next-node)) (flows (second next-node)))]
                [(if added
                   (assoc open-set next-node (+ tentative-gscore heuristic))
                   open-set)
                 goal-score
                 came-from])))))

(defn search-with-elephant [elephant-move? start-time]
  (let [start ["AA" (list) "AA" (list) 0 start-time]
        worst-score (calculate-cost 0 start-time)]
    (loop [[open-set goal-score came-from] [(priority-map start 0) (assoc {} start 0) {}]
           nodes 0
           min-so-far Integer/MAX_VALUE]
      (cond
       (empty? open-set) [goal-score came-from nodes (- worst-score min-so-far)]
        (> nodes 2000000) [goal-score came-from nodes (- worst-score min-so-far)]
       :else (let [[current-node] (peek open-set)
             [location path ele-location ele-path open-valves time-left] current-node
             open-set (pop open-set)
            ;; we need to process both my action and the elephant's action
            ;; IF I open a valve, we move on to the elephant.
            ;; if there's no valve worth opening we generate a list of next
            ;; positions and need to glom the elephants on.
            ;; so it's like, my-next and elephant-next, these are lists, then
            ;; there's some method of merging the lists.
            ;; oh yes, standing pat should also be added.
             stand-pat-node [location (list) ele-location (list) open-valves 0]
             stand-pat-score (+ (goal-score current-node) (calculate-cost open-valves time-left))
            ;; fun, the logic is basically the same.
             [goal-score came-from] (maybe-add-score stand-pat-node current-node stand-pat-score goal-score came-from)
             min-so-far (if (< (get goal-score stand-pat-node) min-so-far)
                          (do (printf "new min %d (scanned %d nodes)\n" (- worst-score stand-pat-score) nodes)
                              (flush)
                              (get goal-score stand-pat-node))
                          min-so-far)]
         (recur
          (reduce
           (process-neighbor current-node)
           [open-set goal-score came-from]
           (combo-lists
            (get-neighbors [location path open-valves time-left])
            (if elephant-move?
              (get-neighbors [ele-location ele-path open-valves time-left])
              [["AA" (list) open-valves (dec time-left)]])))
            ;;  ))
          (inc nodes)
          min-so-far))))))

(defn reconstruct-path-full [node came-from]
  (loop [curr node result []]
    (if (contains? came-from curr)
      (recur (came-from curr) (conj result curr))
      (reverse (conj result curr)))))

(time (println
 "part 1 answer (should be 1651)"
 (let [[goal-score came-from nodes result] (search-with-elephant false 30)
       _ (println "A* search scanned" nodes "nodes")]
   result)))

(time (println
 "part 2 answer (should be 1707)"
 (let [[goal-score came-from nodes result] (search-with-elephant true 26)
       _ (println "A* search scanned" nodes "nodes")]
   result)))
