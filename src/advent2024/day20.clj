(ns advent2024.day20
  (:require
   [grid :as grid]
   [graph :as graph]
   [utils :as utils]
   [clojure.set :as set]))

(defn char-position [grid ch]
  (->> (grid/coords grid) (filter #(= (grid/at grid %) ch)) (first)))

(defn start-coords [grid] (char-position grid \S))
(defn end-coords [grid] (char-position grid \E))

(grid/parse-file "2024/day20-example.txt")

(defn reconstruct [distances start-coords]
  (loop [curr start-coords
         path []]
    (let [d (distances curr)]
      (if (zero? d)
        path
        (recur
         (->> grid/cardinal-directions
             (map #(mapv + curr %))
             (filter #(= (dec d) (distances %)))
             (first))
         (conj path curr))))))

(defn search [grid]
  (let [end-coords (end-coords grid)
        distances
        (graph/dijkstra-search end-coords
                               (fn [c] (grid/neighbors grid c grid/cardinal-directions #(= % \#)))
                               (fn [& _] false))
        path (reconstruct distances (start-coords grid))]
    (->> path
         (mapcat
          (fn [c]
            (->> grid/cardinal-directions
                 (filter (fn [d] (= (grid/at grid (mapv + d c)) \#)))
                 (map #(mapv + % % c))
                 (remove #(nil? (distances %)))
                 (map (fn [cheat-dest]
                        (- (distances c)
                           (distances cheat-dest)
                           2)))
                 (remove #(< % 0))
                 ))))))

(defn answer-part1 [grid]
  (let [saves (search grid)]
    (count (filter #(>= % 100) saves))))

(answer-part1 (grid/parse-file "2024/day20.txt"))

;; part 2 is basically the same instead we can do a bit more wandering through
;; the walls
;; we can BFS or whatever from the inside-the-wall location

(defn search-part2 [grid cheating-fuel]
  (let [end-coords (end-coords grid)
        distances
        (graph/dijkstra-search end-coords
                               (fn [c] (grid/neighbors grid c grid/cardinal-directions #(= % \#)))
                               (fn [& _] false))
        path (reconstruct distances (start-coords grid))]
    (->> path
         (mapcat
          (fn [c]
            (->> grid/cardinal-directions
                 (filter (fn [d] (= (grid/at grid (mapv + d c)) \#)))
              ;; OK we're in the wall, we can walk inside the wall for up to
              ;; 19 more steps
              ;; I'll manually code this since it is fiddly
                 ;; not clear if this is going to explode; BFS 20 steps should
                 ;; be tractable
                 (mapcat
                  (fn [d]
                    (loop [queue [{:coords (mapv + d c) :steps 1}]
                           seen #{}
                           saves {}]
                      (if (empty? queue)
                        (->> (vals saves) (filter #(> % 0)))
                        (let [{:keys [coords steps]} (first queue)]
                          (if (= steps cheating-fuel) ;; cheating fuel is gone
                            (recur (rest queue) (conj seen coords) saves)
                            (let [[queue seen saves]
                              (reduce
                               (fn [[queue seen saves] next]
                                 (cond
                                   (contains? seen next) [queue seen saves]
                                   (not= (grid/at grid next) \#)
                                   [queue seen
                                    (assoc saves next
                                           (- (distances c)
                                              (distances next)
                                              (inc steps)))]
                                   :else
                                   [(conj queue {:coords next :steps (inc steps)})
                                    seen
                                    saves]))
                                [(rest queue) (conj seen coords) saves]
                                (grid/neighbors grid coords))]
                              (recur queue seen saves))))))))))))))

(=
 (frequencies (search-part2 (grid/parse-file "2024/day20-example.txt") 2))
 (frequencies (search (grid/parse-file "2024/day20-example.txt"))))

;; OK great
;; quantity numbers are wonky - claims 22 cheats that save 72, I only see 8
;; this is probably because I'm combining directions
(frequencies (filter #(>= % 50) (search-part2 (grid/parse-file "2024/day20-example.txt") 20)))
