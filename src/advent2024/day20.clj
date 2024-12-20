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
      (conj path end-coords)
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

(defn bfs-neighbor-visit [steps]
  (fn [[queue visited] next]
    (cond
      (contains? visited next) [queue visited]
      :else
      [(conj queue {:coords next :steps (inc steps)})
       (conj visited next)])))

(defn search-part2 [grid cheating-fuel]
  (let [end (end-coords grid)
        distances
        (graph/dijkstra-search
         end
         (fn [c] (grid/neighbors grid c grid/cardinal-directions #(= % \#)))
         (fn [& _] false))
        path (reconstruct distances (start-coords grid) end)]
    (map
     (fn [start]
       (loop [queue [{:coords start :steps 0}]
              visited #{start}
              saves {}]
         (if (empty? queue)
           saves
           (let [{curr :coords steps :steps} (first queue)
                 at-wall? (= \# (grid/at grid curr))
                 saves (if at-wall?
                         saves
                         (assoc saves curr
                                (-
                                 (distances start)
                                 (distances curr)
                                 steps)))]
             (if
              (= steps cheating-fuel) ;; out o'cheating fuel
               (recur
                (subvec queue 1)
                visited
                saves)
               (let [[queue visited]
                     (reduce
                      (bfs-neighbor-visit steps)
                      [(subvec queue 1) visited]
                      (grid/neighbors grid curr))]
                 (recur queue visited saves)))))))
     path)))

(defn count-paths [results]
  (mapcat (fn [s] (->> (vals s) (filter #(> % 0)))) results))

(=
 (frequencies (count-paths (search-part2 (grid/parse-file "2024/day20-example.txt") 2)))
 (frequencies (search (grid/parse-file "2024/day20-example.txt"))))

(count-paths (search-part2 (grid/parse-file "2024/day20-example.txt") 2))

(let [f (frequencies (count-paths  (search-part2 (grid/parse-file "2024/day20-example.txt") 20)))]
  ;; correct
  (assert (= (f 76) 3))
  (assert (= (f 74) 4))
  (assert (= (f 72) 22))
  (assert (= (f 70) 12))
  (assert (= (f 68) 14))
  (assert (= (f 66) 12))
  (assert (= (f 64) 19))
  (assert (= (f 62) 20) (str "value was " (f 62) "; needed " 20))
  (assert (= (f 60) 23) (str "value was " (f 60) "; needed " 23))
  (assert (= (f 58) 25) (str "value was " (f 58) "; needed " 25))
  (assert (= (f 56) 39) (str "value was " (f 56) "; needed " 39))
  (assert (= (f 54) 29) (str "value was " (f 54) "; needed " 29))
  (assert (= (f 52) 31) (str "value was " (f 52) "; needed " 31))
  (assert (= (f 50) 32) (str "value was " (f 50) "; needed " 32)))

(defn answer-part2 [grid]
  (let [saves (count-paths (search-part2 grid 20))]
    (count (filter #(>= % 100) saves))))

(println "time to calculate")
(time (println (answer-part2 (grid/parse-file "2024/day20.txt"))))

;; OK great
;; quantity numbers are wonky
;; quantity numbers are still wonky
