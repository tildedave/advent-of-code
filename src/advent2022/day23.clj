(ns advent2022.day23
  (:require [utils :as utils]
            [clojure.string :as string]))

(def example-lines (utils/read-resource-lines "input/day23-example.txt"))
(def smaller-example-lines (utils/read-resource-lines "input/day23-example2.txt"))
(def input-lines (utils/read-resource-lines "input/day23.txt"))

;; we actually need this to be an infinite grid.
;; so we will make it an associative array.
;; most logic should stay the same.

(defn bounds [grid]
  (let [ymin (reduce min (keys grid))
        ymax (reduce max (keys grid))
        xmin (reduce min (map #(reduce min (keys %)) (vals grid)))
        xmax (reduce max (map #(reduce max (keys %)) (vals grid)))]
    [[xmin xmax] [ymin ymax]]))

(defn parse-grid [lines]
  (into {}
        (->> lines
             (map #(map-indexed vector %))
             (map #(into {} %))
             (map-indexed vector))))

(defn move-dir [[x y] dir]
  (let [[dx dy]
        (case dir
          :north [0 -1]
          :south [0 1]
          :west [-1 0]
          :east [1 0]
          :northeast [1 -1]
          :northwest [-1 -1]
          :southeast [1 1]
          :southwest [-1 1])]
    [(+ x dx) (+ y dy)]))

(defn positions-valid? [grid [x y] dir-list]
  (every?
   (fn [[x y]] (not= (get-in grid [y x]) \#))
   (map (partial move-dir [x y]) dir-list)))

(defn has-no-neighbors? [grid [x y]]
  (every?
   (fn [[x y]] (not= (get-in grid [y x]) \#))
   (map (partial move-dir [x y])
        (list :north :northeast :east
              :southeast :south :southwest
              :west :northwest))))

(defn elf-positions [grid]
  ;; let's try to make this lazy
  (let [[[xmin xmax] [ymin ymax]] (bounds grid)]
    (->> (for [x (range xmin (inc xmax))
               y (range ymin (inc ymax))]
           (if (= (get-in grid [y x]) \#)
             (list [x y])
             nil))
         (apply concat)
         (set))))

(defn round-one [grid elf-pos-list dir-idx]
  ;; so here we're going to keep track of, for each location, which elves
  ;; propose moving to it. that will make part 2 easy.
  ;; OK, create a bunch of maps and merge-with.
  (apply
   merge-with concat
   (for [[x y] elf-pos-list]
     (if
      (has-no-neighbors? grid [x y]) nil
      (loop [i dir-idx
             n 0]
        (cond
          (>= n 4) nil
          (= i 0)
          (if (positions-valid? grid [x y] '(:north :northeast :northwest))
            {(move-dir [x y] :north) [[x y]]}
            (recur (mod (inc i) 4) (inc n)))
          (= i 1)
          (if (positions-valid? grid [x y] '(:south :southeast :southwest))
            {(move-dir [x y] :south) [[x y]]}
            (recur (mod (inc i) 4) (inc n)))
          (= i 2)
          (if (positions-valid? grid [x y] '(:west :northwest :southwest))
            {(move-dir [x y] :west) [[x y]]}
            (recur (mod (inc i) 4) (inc n)))
          (= i 3)
          (if (positions-valid? grid [x y] '(:east :northeast :southeast))
            {(move-dir [x y] :east) [[x y]]}
            (recur (mod (inc i) 4) (inc n)))))))))

(defn new-row [grid]
  (let [max-width (->> grid
                       (vals)
                       (map count)
                       (reduce max))]
    (into {} (map-indexed vector (repeat max-width \.)))))

(assoc-in
 {0 {0 \., 1 \., 2 \., 3 \., 4 \#, 5 \., 6 \.}, 1 {0 \., 1 \., 2 \#, 3 \#, 4 \#, 5 \., 6 \#}, 2 {0 \#, 1 \., 2 \., 3 \., 4 \#, 5 \., 6 \#}, 3 {0 \., 1 \#, 2 \., 3 \., 4 \., 5 \#, 6 \#}, 4 {0 \#, 1 \., 2 \#, 3 \#, 4 \#, 5 \., 6 \.}, 5 {0 \#, 1 \#, 2 \., 3 \#, 4 \., 5 \#, 6 \#}, 6 {0 \., 1 \#, 2 \., 3 \., 4 \#, 5 \., 6 \.}}
 [5 -1]
 \d)


(defn round-two [grid elf-pos-list dir-idx round-one-results]
  ;; for these elves modify the grid.  remove the old one, add the new one.
  (reduce
   (fn [[grid elf-pos-list dir-idx] [[nx ny] [[ox oy]]]]
     (let [grid (if (not (contains? grid ny))
                  (assoc grid ny (new-row grid))
                  grid)]
       [(-> grid
            (assoc-in [ny nx] \#)
            (assoc-in [oy ox] \.))
        (-> elf-pos-list
            (disj [ox oy])
            (conj [nx ny]))
        dir-idx]))
   [grid elf-pos-list (mod (inc dir-idx) 4)]
   (->> round-one-results
        (filter #(= (count (second %)) 1)))))

(assoc-in (parse-grid example-lines) [0 0] \d)

(defn string-grid [grid]
  (let [[[xmin xmax] [ymin ymax]] (bounds grid)]
    (string/join
     "\n"
     (for [y (range ymin (inc ymax))]
       (let [row (for [x (range xmin (inc xmax))]
                   (let [ch (get-in grid [y x] \.)]
                     ch))]
         (string/join row))))))


(defn print-grid [grid]
  (println (string-grid grid)))

;; needs to get rewritten to for
(defn empty-tiles [grid]
  (->> grid
      (string-grid)
      (filter #(= \. %))
       (count)))

(defn prune-to-elves [grid]
  ;; first elf, last elf in each row.
  (let [elf-bounds
        (map
         (fn [[k v-list]]
           (let [elf-pos (->> v-list
                              (filter #(= (second %) \#))
                              (map first))]
             (if (empty? elf-pos)
               [k nil]
               [k [(reduce min elf-pos) (reduce max elf-pos)]])))
         grid)
        x-min-bounds (->> elf-bounds
                          (map second)
                          (remove nil?)
                          (map first))
        x-max-bounds (->> elf-bounds
                          (map second)
                          (remove nil?)
                          (map second))
        x-min (reduce min x-min-bounds)
        x-max (reduce max x-max-bounds)
        ;; for y-min / y-max we do a map-indexed on the bounds and see which
        ;; is the first/last  one that's not -1 -1]
        y-bounds (->> elf-bounds
                      (remove (fn [[_ v-list]] (nil? v-list)))
                      (map first)
                      (sort))
        y-min (first y-bounds)
        y-max (last y-bounds)]
    ;; [[x-min x-max] [y-min y-max]]
    (->> grid
         (remove (fn [[y _]] (or (< y y-min) (> y y-max))))
         (map (fn [[y v-list]]
                [y (into {} (remove (fn [[x _]] (or (< x x-min) (> x x-max))) v-list))]))
         (into {}))))

(prune-to-elves (parse-grid smaller-example-lines))

(defn elf-sequence [lines]
  (let [grid (parse-grid lines)]
    (iterate
     (fn [[grid elf-pos-list dir-idx]]
       (round-two
        grid elf-pos-list dir-idx
        (round-one grid elf-pos-list dir-idx)))
     [grid
      (elf-positions grid)
      0])))

;; answer 1
(-> (elf-sequence input-lines)
    (nth 10)
    (first)
    (prune-to-elves)
    (empty-tiles))

(nth (elf-sequence example-lines) 1)

(let [elf-seq (elf-sequence input-lines)]
  (loop [i 0]
    (let [[_ elf1] (nth elf-seq i)
          [_ elf2] (nth elf-seq (inc i))]
      (if (= elf1 elf2) (println "Done at" (inc i))
          (recur (inc i))))))
