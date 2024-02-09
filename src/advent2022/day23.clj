(ns advent2022.day23
  (:require [advent2022.utils :as utils]
            [clojure.string :as string]))

(def example-lines (utils/read-resource-lines "input/day23-example.txt"))
(def smaller-example-lines (utils/read-resource-lines "input/day23-example2.txt"))
(def input-lines (utils/read-resource-lines "input/day23.txt"))

(defn parse-grid [lines]
  (->> lines
     (mapv vec)))

(defn valid-coord? [grid [x y]]
  (and (>= y 0)
       (< y (count grid))
       (>= x 0)
       (< x (count (first grid)))))

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
   (fn [[x y]] (= (get-in grid [y x]) \.))
   (map (partial move-dir [x y]) dir-list)))

(defn has-no-neighbors? [grid [x y]]
  (every?
  (fn [[x y]] (not= (get-in grid [y x]) \#))
  (map (partial move-dir [x y])
       (list :north :northeast :east
             :southeast :south :southwest
             :west :northwest))))

(apply concat '((1) nil (2)))

(defn elf-positions [grid]
  ;; let's try to make this lazy
  (apply
   concat
   (for [x (range 0 (count (first grid)))
         y (range 0 (count grid))]
     (if (= (get-in grid [y x]) \#)
       (list [x y])
       nil))))

(defn round-one [grid dir-idx]
  ;; so here we're going to keep track of, for each location, which elves
  ;; propose moving to it. that will make part 2 easy.
  ;; OK, create a bunch of maps and merge-with.
  (let [elf-pos-list (elf-positions grid)]
    (apply
     merge-with concat
     (for [[x y] elf-pos-list]
       (if
        (has-no-neighbors? grid [x y]) nil
          (loop [i dir-idx
                 n 0]
          (cond
            (> n 4) nil
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
              (recur (mod (inc i) 4) (inc n)))
            )))))))

(defn round-two [grid round-one-results]
  ;; for these elves modify the grid.  remove the old one, add the new one.
  (reduce
   (fn [grid [[nx ny] [[ox oy]]]]
     (-> grid
         (assoc-in [ny nx] \#)
         (assoc-in [oy ox] \.)))
   grid
   (->> round-one-results
        (filter #(= (count (second %)) 1)))))

(round-one (parse-grid smaller-example-lines) 0)

(defn print-grid [grid]
  (println (string/join "\n" (map string/join grid))))

(defn empty-tiles [grid]
  (->> grid
       (map (fn [l] (filter #(= % \.) l)))
       (map count)
       (reduce + 0)))

(defn prune-to-elves [grid]
  ;; first elf, last elf in each row.
  (let [elf-bounds (map #(vector (.indexOf % \#) (.lastIndexOf % \#)) grid)
        x-min-bounds (remove #(= % -1) (map first elf-bounds))
        x-max-bounds (map second elf-bounds)
        x-min (reduce min x-min-bounds)
        x-max (reduce max x-max-bounds)
        ;; for y-min / y-max we do a map-indexed on the bounds and see which
        ;; is the first/last  one that's not -1 -1]
        y-bounds (remove #(= (first (second %)) -1) (map-indexed vector elf-bounds))
        [y-min] (first y-bounds)
        [y-max] (last y-bounds)]
    [[x-min x-max] [y-min y-max]]
    (map #(subvec % x-min (inc x-max))
         (subvec grid y-min (inc y-max)))))

(empty-tiles (parse-grid smaller-example-lines))

(-> (iterate
     (fn [[grid dir-idx]]
       [(round-two grid (round-one grid dir-idx)) (mod (inc dir-idx) 4)])
     [(parse-grid example-lines) 0])
    (nth 10)
    (first)
    (prune-to-elves)
    (print-grid))
