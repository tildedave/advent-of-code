(ns advent2022.day16
  (:require [advent2022.utils :as utils]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

;; I guess we start with floyd warshall, eliminate the 00 flow nodes, and
;; perform some kind of search.

(def lines (utils/read-resource-lines "input/day16.txt"))

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

;; I suppose we can do an exhaustive search now.
;; part 2 will probably be bad, whatever he asks us to do.
;; we're only searching for 30 minutes so it seems reasonable to brute force.
;; how can we cut-off?  if we end up at a certain location with the same
;; number of open valves - this is actually very similar to the key problem
;; from AOC 2019.
;; the puzzle input is also < 64.  so it seems like this is a good direction.
;; state - [location valves-open pressure-released minutes]
;; challenge: pressure and minutes left are related but not directly.  it's
;; not enough to release a valve as the time left matters.
;; well, the minute we release the pressure we can assume that we get credit
;; for the entire rest of the time period.
;; with the floyd-warshall distances, everything is connected to everything
;; else with accurate times.
;; I guess the numbers are relatively small, and 30 minutes is reasonable.
;; let's just code this thing.

;; I guess let's try a greedy approach for part 1.
;; greedy approach doesn't work of course.
;; we can easily brute force the example but it's not easy to understand how
;; to brute force the rest.

;; for any sequence we can understand the score.
;; this is actually doable if we can cut off when we're done, e.g. multiply
;; all remaining flows by time left and bail.
(defn score [valve-ordering distances flows]
  (loop [location "AA"
         time-left 30
         score 0
         locations valve-ordering]
    (if (empty? locations)
      score
      (let [[next-location] locations]
        (if (< time-left 0) score
            (let [next-time-left (- time-left 1 (distances [location next-location]))
                  next-score (+ score (* next-time-left (flows next-location)))]
              (recur next-location next-time-left next-score (rest locations))))))))


;; example set has 7, 7! is brute forceable (5420 or something, super EZ)
;; actual problem has 16, 16! is not brute forceable.

(count (combo/permutations (apply hash-set (filter #(> (flows %) 0) (keys flows)))))

(let [interesting-valves (apply hash-set (filter #(> (flows %) 0) (keys flows)))
      distances (all-pairs-shortest-paths adjacency-matrix)]
  (apply max (map #(score % distances flows) (take 100 (combo/permutations interesting-valves)))))

