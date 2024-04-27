(ns grid
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn parse
  ([lines] (parse lines identity))
  ([lines sq-f]
  (->> lines
       (map seq)
       (map #(mapv sq-f %))
       (vec))))

(defn bounds [grid]
  [(count (first grid))
   (count grid)])

(defn out-of-bounds? [grid [x y]]
  (or (< y 0)
      (>= y (count grid))
      (< x 0) (>= x (count (first grid)))))

(def cardinal-directions [[-1 0] [1 0] [0 -1] [0 1]])
(def ordinal-directions [[-1 -1] [-1 1] [1 -1] [1 1]])
(def all-directions (concat cardinal-directions ordinal-directions))

(defn add [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn neighbors
  ([grid [x y]] (neighbors grid [x y] cardinal-directions))
  ([grid [x y] directions]
   (remove
    nil?
    (for [[dx dy] directions]
      (let [[nx ny] [(+ x dx) (+ y dy)]]
        (if (out-of-bounds? grid [nx ny])
          nil
          [nx ny]))))))

(defn at [grid [x y]]
  (get-in grid [y x]))

(defn coords [grid]
  (let [xmax (count (first grid))
        ymax (count grid)]
    (for [x (range 0 xmax)
          y (range 0 ymax)]
      [x y])))


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
         (is-goal? current) (goal-score (state-hash current))
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
      visited
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
