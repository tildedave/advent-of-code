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

(defn at [grid [x y]]
  (get-in grid [y x]))

(defn neighbors
  ([grid [x y]] (neighbors grid [x y] cardinal-directions))
  ([grid [x y] directions] (neighbors grid [x y] directions (fn [_] false)))
  ([grid [x y] directions is-wall?]
   (remove
    nil?
    (for [[dx dy] directions]
      (let [[nx ny] [(+ x dx) (+ y dy)]]
        (cond
          (out-of-bounds? grid [nx ny]) nil
          (is-wall? (at grid [nx ny])) nil
          :else [nx ny]))))))

(defn coords [grid]
  (let [xmax (count (first grid))
        ymax (count grid)]
    (for [x (range 0 xmax)
          y (range 0 ymax)]
      [x y])))

(defn is-adjacent?
  ([[x1 y1] [x2 y2]] (is-adjacent? [x1 y1] [x2 y2] cardinal-directions))
  ([[x1 y1] [x2 y2] directions]
   (->>
    (for [[dx dy] directions]
      (if (= [(+ x1 dx) (+ y1 dy)] [x2 y2])
        true
        false))
    (filter true?)
    (empty?)
    (not))))


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

   (defn breadth-first-search [start end neighbors should-cutoff?]
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
                     (neighbors current))))))))

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
