(ns advent2022.day24
  (:require [utils :as utils]
            [clojure.string :as string]
            [clojure.data.priority-map :refer [priority-map]]))

(def example-lines (utils/read-resource-lines "input/day24-example.txt"))
(def example2-lines (utils/read-resource-lines "input/day24-example2.txt"))
(def input-lines (utils/read-resource-lines "input/day24.txt"))

(defn parse-grid [lines]
  (->> lines
       (mapv vec)))

(defn string-grid [grid]
  (string/join "\n" (map string/join grid)))

(defn is-blizzard? [ch]
  (case ch
    \> true
    \< true
    \v true
    \^ true
    false))

(defn blizzard-positions [grid]
  (->> grid
       (map-indexed vector)
       (map (fn [[y x-list]] [y (->> x-list
                                     (map-indexed vector)
                                     (filter #(is-blizzard? (second %))))]))
       ;; must handle multiple rows.
       (remove #(empty? (second %)))
       (mapcat (fn [[y x-list]]
                 (map (fn [[x ch]] [[x y] #{ch}]) x-list)))
       (into {})))

(blizzard-positions (parse-grid example-lines))
    ;;    (map-indexed vector)))

    ;;    (filter #(is-blizzard? (second %)))))

(defn reverse-blizzard [ch]
  (case ch
    \^ \v
    \v \^
    \< \>
    \> \<))

(defn tick [grid positions]
  (reduce
   (fn [positions [[x y] ch]]
     (let [[dx dy] (case ch
                     \^ [0 -1]
                     \> [1 0]
                     \< [-1 0]
                     \v [0 1])
           [nx ny] [(+ x dx) (+ y dy)]
           [nx ny] (if (= (get-in grid [ny nx]) \#)
                     [(mod (+ x (* 3 dx)) (count (first grid)))
                      (mod (+ y (* 3 dy)) (count grid))]
                     [nx ny])
           ]
       (-> positions
           (update [nx ny] (fn [s] (conj (if (nil? s) #{} s) ch))))))
   {}
   (apply concat
          (for [[x y] (keys positions)]
            (for [ch (positions [x y])]
              [[x y] ch])))))

(defn print-grid
  ([grid positions] (print-grid grid positions nil))
  ([grid positions curr]
  (->> (for [y (range 0 (count grid))]
         (for [x (range 0 (count (first grid)))]
           (cond
            (= (get-in grid [y x]) \#) \#
            (= [x y] curr) "E"
            :else
            (let [pos-set (get positions [x y] #{})]
              (condp = (count pos-set)
                0 \.
                1 (first pos-set)
                (count pos-set))))))
       (map string/join)
       (string/join "\n")
       (println))))

;; (print-grid (parse-grid example-lines) (blizzard-positions (parse-grid example-lines)))

(defn blizzard-seq [grid]
  (iterate #(vector grid (apply tick %)) [grid (blizzard-positions grid)]))

(defn positions [grid]
  (let [dot-idx (fn [line] (->> line (map-indexed vector)
                         (filter #(not= (second %) \#))
                         (map first)))]
    (->> grid
         (map dot-idx)
         (map-indexed vector)
         (mapcat (fn [[y x-list]] (map #(vector % y) x-list))))))

(defn bounds [grid]
  (let [dot-idx (fn [line] (->> line (map-indexed vector)
                                (filter #(= (second %) \.))
                                (first)
                                (first)))]
    [[(->> grid (first) (dot-idx)) 0]
     [(->> grid (last) (dot-idx)) (dec (count grid))]]))

(bounds (parse-grid example2-lines))

(defn neighbors [[[x y] t trip-num] blizzard-cycle-time grid blizzard-positions]
   ;; NOTE: you can't wait if you're going to be hit by a blizzard.
  (->> (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1] [0 0]]]
         [(+ x dx) (+ y dy)])
       (remove #(contains? blizzard-positions %))
       (remove #(nil? (get-in grid [(second %) (first %)] nil)))
       (map #(vector % (mod (inc t) blizzard-cycle-time) trip-num))))

;; (positions (parse-grid example2-lines))

(defn cycle-length [grid]
  (let [blizzard-by-time (blizzard-seq grid)]
    (loop [i 0
           seen {}]
      (let [pos (second (nth blizzard-by-time i))]
      (cond
        (contains? seen pos) (- i (seen pos))
        (> i 50000) -1
        :else (recur (inc i) (assoc seen pos i)))))))

;; (println "Cycle length" (cycle-length (parse-grid example2-lines)))

(defn search
  ([lines] (search lines false))
  ([lines part2?]
  (let [grid (parse-grid lines)
        blizzard-by-time (blizzard-seq grid)
        queue (priority-map)
        [start end] (bounds grid)
        blizzard-cycle-time (cycle-length grid)]
    (loop [[queue distances nodes] [(assoc queue [start 0 0] 0) {} 0]]
      (cond
        (empty? queue)
        (println "error, queue should not have been empty")
        (> nodes 10000000) (println "too many nodes")
        :else
        (let [current (peek queue)
              [[[x y] nt trip-num] t] current
              ;; actually we can just mod the cycle time.
              blizzard-pos (second (nth blizzard-by-time (mod (inc t) blizzard-cycle-time)))
              queue (pop queue)]
          (cond
            (and (= [x y] end) (not part2?))
                ;; done
                t
            (and (= [x y] end)
                 (not= trip-num 1)
                 part2?)
            (case trip-num
              0 (recur [(assoc queue [[x y] nt (inc trip-num)] t) distances (inc nodes)])
              ;; we did it!
              2 t)
              ;; we'll just recur with trip-num 1.
            (and (= [x y] start) (= trip-num 1))
            (recur [(assoc queue [[x y] nt (inc trip-num)] t) distances (inc nodes)])
            :else
            (recur
             (reduce
              (fn [[queue distances nodes] neighbor]
                (let [[[nx ny] nt] neighbor
                      ;; nt is normalized, t is real.
                      ;; normalized is only important for queue memoization.
                      ;; when we put neighbor on the priority map, needs to be
                      ;; with real distance.
                      alt (inc t)]
                  (cond
                    (= (get-in grid [ny nx] \#) \#) [queue distances nodes]
                    (< alt (get distances neighbor Integer/MAX_VALUE))
                    ;; yes
                    [(assoc queue neighbor alt)
                     (assoc distances neighbor alt)
                     nodes]
                    :else
                    [queue distances nodes])))
              [queue distances (inc nodes)]
              (neighbors (first current) blizzard-cycle-time grid blizzard-pos))))))))))

(println "cycle length" (cycle-length (parse-grid input-lines)))
(println "part 1 answer" (search input-lines))
(println "part 2 answer" (search input-lines :part2))

;; (println "testing")
;; (let [[g pos] (-> (blizzard-seq (parse-grid example2-lines))
;;            (nth 1))]
;;   (print-grid g pos))
