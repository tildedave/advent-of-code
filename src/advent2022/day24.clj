(ns advent2022.day24
  (:require [advent2022.utils :as utils]
            [clojure.string :as string]
            [clojure.data.priority-map :refer [priority-map]]))

(def example-lines (utils/read-resource-lines "input/day24-example.txt"))
(def example2-lines (utils/read-resource-lines "input/day24-example2.txt"))

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

(defn print-grid [grid positions]
  (->> (for [y (range 0 (count grid))]
         (for [x (range 0 (count (first grid)))]
           (if
            (= (get-in grid [y x]) \#) \#
            (let [pos-set (get positions [x y] #{})]
              (condp = (count pos-set)
                0 \.
                1 (first pos-set)
                (count pos-set))))))
       (map string/join)
       (string/join "\n")
       (println)))

(print-grid (parse-grid example-lines) (blizzard-positions (parse-grid example-lines)))

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
    [[0 (->> grid (first) (dot-idx))]
     [(dec (count grid)) (->> grid (last) (dot-idx))]]))

(bounds (parse-grid example-lines))

(defn neighbors [[x y] t grid blizzard-positions]
   ;; NOTE: you can't wait if you're going to be hit by a blizzard.
  (->> (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1] [0 0]]]
         [(+ x dx) (+ y dy)])
       (remove #(contains? blizzard-positions %))
       (remove #(nil? (get-in grid [(second %) (first %)] nil)))
       (map #(vector % (inc t)))))

(positions (parse-grid example2-lines))

(use 'clojure.tools.trace)
;; I guess this is A* search again
(defn search [lines]
  (let [grid (parse-grid lines)
        blizzard-by-time (blizzard-seq grid)
        queue (priority-map)
        [start end] (bounds grid)]
    (loop [[queue goal-score nodes] [(assoc queue start 0)
                                     {[start 0] 0}
                                     0]]
      (cond
        (empty? queue)
        (println "error, queue should not have been empty")
        (> nodes 100000) (println "too many nodes")
        :else
        (let [current (peek queue)
              [[x y] t] current
              ;; nth is probably fine here unless time gets very high.
              blizzard-pos (second (nth blizzard-by-time (inc t)))
              queue (pop queue)]
          (if (= [x y] end)
            ;; we did it!
            t
            (recur
              (reduce
              (fn [[queue goal-score nodes] neighbor]
                (let [[[nx ny] t] neighbor
                      tentative-gscore (+ (goal-score current) 1)
                       current-gscore (get goal-score neighbor Integer/MAX_VALUE)]
                  (cond
                    (= (get-in grid [ny nx] \#) \#) [queue goal-score nodes]
                    (< tentative-gscore current-gscore)
                    [(assoc queue [nx ny] t)
                     (assoc goal-score neighbor tentative-gscore)
                     nodes]
                    :else [queue goal-score nodes])))
              [queue goal-score (inc nodes)]
              (neighbors [x y] t grid blizzard-pos)))))))))

(println "example2" (search example-lines))

(println "testing")
(let [[g pos] (-> (blizzard-seq (parse-grid example2-lines))
           (nth 1))]
  (print-grid g pos))
