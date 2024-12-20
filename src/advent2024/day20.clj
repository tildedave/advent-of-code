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

(defn reconstruct [distances start-coords end-coords]
  (loop [curr start-coords
         path []]
    (if (= curr end-coords)
      path
      (let [d (distances curr)]
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
        path (reconstruct distances (start-coords grid) end-coords)]
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

(defn take-n-steps [distances start-coords end-coords n]
  (loop [curr start-coords
         total-steps 0]
    (if (or (= curr end-coords) (= total-steps n))
      curr
      (let [d (distances curr)]
        (recur
         (->> grid/cardinal-directions
              (map #(mapv + curr %))
              (filter #(= (dec d) (distances %)))
              (first))
         (inc total-steps))))))

(defn bfs-neighbor-visit [grid steps start end distances cheating-fuel]
  (fn [[queue visited saves] next]
    (cond
      (contains? visited next) [queue visited saves]
      (not= (grid/at grid next) \#)
      [(conj queue {:coords next :steps (inc steps)})
       (conj visited next)
       (let [save (- (distances start)
                     (distances next)
                     (inc steps))]
         ;; this coord actually needs to mark the end coord _as if_ we followed the closest path.  the save is still what we calculated.
         (update
          saves
          (take-n-steps distances next end (- cheating-fuel (inc steps)))
          (fnil #(conj % save) #{})))]
      :else
      [(conj queue {:coords next :steps (inc steps)})
       (conj visited next)
       saves])))

(defn search-part2 [grid cheating-fuel]
  (let [end (end-coords grid)
        distances
        (graph/dijkstra-search end
                               (fn [c] (grid/neighbors grid c grid/cardinal-directions #(= % \#)))
                               (fn [& _] false))
        path (reconstruct distances (start-coords grid) end)]
    (->> path
        ;;  (take 1)
         (mapcat
          (fn [start]
            (loop [queue (->> grid/cardinal-directions
                              (filter (fn [d] (= (grid/at grid (mapv + d start)) \#)))
                              (mapv (fn [d] {:coords (mapv + d start) :steps 1})))
                   visited #{}
                   saves {}]
              (if (empty? queue)
                (->> saves (vals) (reduce set/union) (filter #(> % 0)))
                (let [{:keys [coords steps]} (first queue)]
                  (if (= steps cheating-fuel) ;; cheating fuel is gone
                    (recur
                     (subvec queue 1)
                     visited
                     saves)
                    (let [[queue visited saves]
                          (reduce
                           (bfs-neighbor-visit grid steps start end distances cheating-fuel)
                           [(subvec queue 1) visited saves]
                           (grid/neighbors grid coords))]
                      (recur queue visited saves)))))))))))

(=
 (frequencies (search-part2 (grid/parse-file "2024/day20-example.txt") 2))
 (frequencies (search (grid/parse-file "2024/day20-example.txt"))))

;; OK great
;; quantity numbers are wonky

;; quantity numbers are still wonky
(frequencies (search-part2 (grid/parse-file "2024/day20-example.txt") 20))


(filter (fn [[_ v]] (contains? v 76)) (first (search-part2 (grid/parse-file "2024/day20-example.txt") 20)))

(end-coords (grid/parse-file "2024/day20-example.txt"))
