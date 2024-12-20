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
         (take 1)
         (mapcat
          (fn [c]
            (loop [queue (->> grid/cardinal-directions
                              (filter (fn [d] (= (grid/at grid (mapv + d c)) \#)))
                              (mapv (fn [d] {:coords (mapv + d c) :steps 1})))
                   visited #{}
                   saves {}]
              (println queue)
              (if (empty? queue)
                (->> (vals saves) (filter #(> % 0)))
                (let [{:keys [coords steps]} (first queue)
                      _ (println "we are at" coords)]
                  (if (= steps cheating-fuel) ;; cheating fuel is gone
                    (recur (subvec queue 1) (conj visited coords) saves)
                    (let [[queue visited saves]
                          (reduce
                           (fn [[queue visited saves] next]
                             (cond
                               (contains? visited next) [queue visited saves]
                               (not= (grid/at grid next) \#)
                               [queue visited
                                (let [possible-save (- (distances c)
                                                       (distances next)
                                                       (inc steps))]
                                  (if (> possible-save (get saves next Integer/MIN_VALUE))
                                    (assoc saves next possible-save)
                                    saves))]
                               :else
                               [(conj queue {:coords next :steps (inc steps)})
                                (conj visited next)
                                saves]))
                           [(subvec queue 1) visited saves]
                           (grid/neighbors grid coords))]
                      (recur queue visited saves)))))))))))

(=
 (frequencies (search-part2 (grid/parse-file "2024/day20-example.txt") 2))
 (frequencies (search (grid/parse-file "2024/day20-example.txt"))))

;; OK great
;; quantity numbers are wonky - claims 22 cheats that save 72, I only see 8
;; this is probably because I'm combining directions
(search-part2 (grid/parse-file "2024/day20-example.txt") 6)
