(ns advent2024.day16
  (:require
   [grid :as grid]
   [graph :as graph]
   [utils :as utils]))

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
(defn neighbors [grid n]
  (let [{:keys [coords direction]} n]
  (let [turns (list {:coords coords :direction (turn-left direction) :cost 1000}
                    {:coords coords :direction (turn-right direction) :cost 1000})]
  (if (empty? (grid/neighbors grid coords [direction] #(= % \#)))
    turns
    (cons {:coords (mapv + coords direction) :direction direction :cost 1} turns)))))

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
   (fn [_ n] (:cost n))))

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

