(ns advent2024.day16
  (:require
   [grid :as grid]
   [graph :as graph]
   [utils :as utils]
   [clojure.set :as set]))

(def example-grid
  '("###############"
    "#.......#....E#"
    "#.#.###.#.###.#"
    "#.....#.#...#.#"
    "#.###.#####.#.#"
    "#.#.#.......#.#"
    "#.#.#####.###.#"
    "#...........#.#"
    "###.#.#####.#.#"
    "#...#.....#.#.#"
    "#.#.#.###.#.#.#"
    "#.....#...#.#.#"
    "#.###.#.#.#.#.#"
    "#S..#.....#...#"
    "###############"))

(defn turn-left [direction]
  (case direction
    [1 0] [0 -1]
    [-1 0] [0 1]
    [0 -1] [-1 0]
    [0 1] [1 0]))

(defn turn-right [direction]
  (-> direction (turn-left) (turn-left) (turn-left)))


;; go forward, cost 1
;; turn left or right, cost 1000

(defn can-go-forward? [grid {:keys [coords direction]}]
  (seq (grid/neighbors grid coords [direction] #(= % \#))))

(defn neighbors [grid {:keys [coords direction] :as x}]
  (let [turns (list {:coords coords :direction (turn-left direction) :cost 1000}
                    {:coords coords :direction (turn-right direction) :cost 1000})]
    (if (can-go-forward? grid x)
      (cons {:coords (mapv + coords direction) :direction direction :cost 1} turns)
      turns)))

(neighbors (grid/parse example-grid) {:coords [1 1] :direction [1 0]})

(defn start-location [grid]
  (->> (grid/coords grid) (filter #(= (grid/at grid %) \S)) (first)))

(defn end-location [grid]
  (->> (grid/coords grid) (filter #(= (grid/at grid %) \E)) (first)))

(start-location (grid/parse example-grid))
(let [grid (grid/parse example-grid)
      start-location (start-location grid)
      end-location (end-location grid)]
  (graph/a*-search
   {:coords start-location :direction [1 0]}
   #(= (:coords %) end-location)
   (partial neighbors grid)
   ;; heuristic doesn't seem to help right now
   (fn [{:keys [coords]}] (-
                           (utils/manhattan-distance start-location end-location)
                           (utils/manhattan-distance end-location coords)))
;;    (fn [& args] 1000)
   (fn [_ n] (:cost n))
   (fn [x] (dissoc x :cost))))

(defn solve-grid [grid]
  (let [start-location (start-location grid)
        end-location (end-location grid)]
    (graph/a*-search
     {:coords start-location :direction [1 0]}
     #(= (:coords %) end-location)
     (partial neighbors grid)
   ;; heuristic doesn't seem to help right now
     (fn [{:keys [coords]}] (-
                             (utils/manhattan-distance start-location end-location)
                             (utils/manhattan-distance end-location coords)))
;;    (fn [& args] 1000)
     (fn [_ n] (:cost n)))))

(solve-grid (grid/parse example-grid))
(solve-grid (grid/parse-file "2024/day16.txt"))

;; part 2 idea: solve the grid to get a cutoff
;; queue-based search from current location, but combine left/right forward etc
(defn all-tiles-part-of-best-path [grid]
  (let [start-location (start-location grid)
        end-location (end-location grid)
        [end-cutoff _ _ goal-score] (solve-grid grid)
        goal-score (update-keys goal-score #(dissoc % :cost))]
    (loop [queue [{:coords start-location :direction [1 0] :total-cost 0 :seen #{}}]
           all-seen #{end-location}]
      (if-let [x (first queue)]
        ;; beep boop
        (let [hashed-state (select-keys x [:coords :direction])]
          (cond
            (and (= (:coords x) end-location)
                 (= end-cutoff (:total-cost x)))
            (recur (rest queue) (set/union all-seen (:seen x)))
            (or (> (:total-cost x)
                   end-cutoff)
                (> (:total-cost x)
                   (get goal-score hashed-state Integer/MAX_VALUE)))
            ;; no reason to continue, we cutoff
            (recur (rest queue) all-seen)
            ;; OK we will do all the logic
            :else
            (recur
             (reduce
              conj
              (rest queue)
              (remove
               nil?
               (list
                (when (can-go-forward? grid hashed-state)
                  (-> x
                      (update :total-cost inc)
                      (update :seen #(conj % (:coords x)))
                      (update :coords #(mapv + % (:direction x)))))
                (when (can-go-forward? grid (update hashed-state :direction turn-left))
                  (-> x
                      (update :total-cost (partial + 1001))
                      (update :seen #(conj % (:coords x)))
                      (update :coords #(mapv + % (turn-left (:direction x))))
                      (update :direction turn-left)))
                (when (can-go-forward? grid (update hashed-state :direction turn-right))
                  (-> x
                      (update :total-cost (partial + 1001))
                      (update :seen #(conj % (:coords x)))
                      (update :coords #(mapv + % (turn-right (:direction x))))
                      (update :direction turn-right))))))
             all-seen)))
        all-seen))))

(defn enrich-grid [grid seen]
  (reduce
   (fn [grid coords] (grid/assoc grid coords \O))
   grid
   seen))

(let [grid (grid/parse example-grid)]
  (enrich-grid grid (all-tiles-part-of-best-path grid)))

(def second-example
  '("#################"
    "#...#...#...#..E#"
    "#.#.#.#.#.#.#.#.#"
    "#.#.#.#...#...#.#"
    "#.#.#.#.###.#.#.#"
    "#...#.#.#.....#.#"
    "#.#.#.#.#.#####.#"
    "#.#...#.#.#.....#"
    "#.#.#####.#.###.#"
    "#.#.#.......#...#"
    "#.#.###.#####.###"
    "#.#.#...#.....#.#"
    "#.#.#.#####.###.#"
    "#.#.#.........#.#"
    "#.#.#.#########.#"
    "#S#.............#"
    "#################"))

(count (all-tiles-part-of-best-path (grid/parse second-example)))
(time (count (all-tiles-part-of-best-path (grid/parse-file "2024/day16.txt"))))
