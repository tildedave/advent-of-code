(ns advent2019.day20
  (:require [grid :as grid]
            [graph :as graph]
            [utils :as utils]
            [clojure.math.combinatorics :as combo]))

;; so grid bounds don't work but whatever.

(defn is-portal? [grid coord]
  (if-let [x (grid/at grid coord)]
    (Character/isAlphabetic (int x))
    false))

(defn is-exterior-portal?
  "Exterior portals have at least one of the 4 range that's nil,
   I guess?"
  [grid coord1 coord2]
  (let [delta (mapv - coord1 coord2)]
    (->>
     (list
      (mapv + coord1 delta)
      (mapv + coord2 delta)
      (mapv + coord1 (mapv - delta))
      (mapv + coord2 (mapv - delta)))
     (map #(grid/at grid %))
     (filter nil?)
     (empty?)
     (not))))

(is-exterior-portal?
 (grid/parse-file "2019/day20-example.txt")
 [0 9] [1 9])

(defn is-adjacent? [coord1 coord2]
  (= (utils/manhattan-distance coord1 coord2) 1))

(defn portal-coord [grid coord1 coord2]
  ;; if the portals are vertically aligned, it's the one either above or below it.
  ;; if the portals are horizontally aligned, it's the one either left or right.
  (let [delta (mapv - coord1 coord2)]
    (->>
     (list
     (mapv + coord1 delta)
     (mapv + coord2 delta)
     (mapv + coord1 (mapv - delta))
     (mapv + coord2 (mapv - delta)))
     (filter #(= \. (grid/at grid %)))
     (first))))

(defn portals [grid]
  (let [alpha-coords (filter (partial is-portal? grid) (grid/coords grid))]
    (->> (combo/combinations alpha-coords 2)
         (filter #(apply is-adjacent? %))
         ;; this sorting seems wrong since it is sorting the numbers, not
         ;; sorting the letters.  I guess things match up OK by accident?
         ;; I guess for number-based coord storing the top / left one is first.
         (map (fn [[c1 c2]] (vec (sort [c1 c2]))))
         (map (fn [[c1 c2]]
                {[(grid/at grid c1) (grid/at grid c2)]
                 {(if (is-exterior-portal? grid c1 c2)
                  :exterior
                  :interior)
                  (portal-coord grid c1 c2)}}))
         (reduce (partial merge-with merge) {})
         )))

(defn reverse-portals [portals]
  (reduce merge (for [[k vlist] portals]
    (reduce merge
            (for [v (vals vlist)]
      {v k})))))

(->> ((portals (grid/parse-file "2019/day20-example.txt")) [\B \C])
     (filter (fn [[_ x]] (= x [9 6])))
     (map first)
     (first))

(reverse-portals (portals (grid/parse-file "2019/day20-example.txt")))

;; so part 1 is just dijkstra search
;; part 2 is also just dijkstra search
;; our dijkstra search doesn't stop at the end so we'll use our A* search,
;; which does.

(defn minimum-path [grid]
  (let [portals (portals grid)
        reverse-portals (reverse-portals portals)
        start (:exterior (portals [\A \A]))
        end (:exterior (portals [\Z \Z]))]
    (graph/a*-search
     start
     (partial = end)
     (fn [current]
       ;; neighbors are our neighbors as per normal grid, and, if we're in a
       ;; portal, then the other node
       (let [walking-neighbors (grid/neighbors grid current grid/cardinal-directions (fn [ch] (not= ch \.)))]
         (if (contains? reverse-portals current)
           (if-let [warp-neighbor (first (filter (partial not= current) (vals (portals (reverse-portals current)))))]
             (cons warp-neighbor walking-neighbors)
             walking-neighbors)
           walking-neighbors)))
     (fn [& _] 0) ;; no heuristic
     (fn [& _] 1) ;; everything is 1 step
     )))

;; part 1
(minimum-path (grid/parse-file "2019/day20-example.txt"))
(minimum-path (grid/parse-file "2019/day20.txt"))

;; so the next part is a bit more annoying with the grid parsing.

(defn warp-neighbor [portals reverse-portals coords depth]
  (if-let [portal-type (->> (portals (reverse-portals coords))
                         (filter (fn [[_ x]] (= x coords)))
                         (first)
                         (first))]
    (let [other-type (case portal-type :exterior :interior :interior :exterior)
          paired-coord (get-in portals [(reverse-portals coords) other-type])]
      (cond
        (and (= depth 0) (= portal-type :exterior)) nil
        (nil? paired-coord) nil
        :else {:coords paired-coord :depth (case portal-type :interior (inc depth) :exterior (dec depth))}))
    nil))

(defn minimum-path-part2 [grid]
  (let [portals (portals grid)
        reverse-portals (reverse-portals portals)
        ;; in all the examples AA/ZZ are exterior.
        start {:coords (:exterior (portals [\A \A])) :depth 0}
        end {:coords (:exterior (portals [\Z \Z])) :depth 0}]
    (graph/a*-search
     start
     (partial = end)
     (fn [{:keys [coords depth]}]
       ;; neighbors are our neighbors as per normal grid, and, if we're in a
       ;; portal, then the other node
       (let [walking-neighbors
             (map (fn [coords] {:coords coords :depth depth})
                  (grid/neighbors grid coords grid/cardinal-directions (fn [ch] (not= ch \.))))]
         (if (contains? reverse-portals coords)
           (if-let [warp-neighbor (warp-neighbor portals reverse-portals coords depth)]
             (cons warp-neighbor walking-neighbors)
             walking-neighbors)
           walking-neighbors)))
     (fn [& _] 0) ;; no heuristic
     (fn [& _] 1) ;; everything is 1 step
     )))

(minimum-path-part2 (grid/parse-file "2019/day20-example3.txt"))
(time (println (minimum-path-part2 (grid/parse-file "2019/day20.txt"))))
