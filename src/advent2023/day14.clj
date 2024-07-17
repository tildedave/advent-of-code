(ns advent2023.day14
  (:require [grid :as grid]
            [utils :as utils]))

;; I did some horrible bitmasking for golang but I'm not doing that now.
;; we will just have the lil rocks go as far as they'll go.

(def example-grid
  '("O....#...."
    "O.OO#....#"
    ".....##..."
    "OO.#O....O"
    ".O.....O#."
    "O.#..O.#.#"
    "..O..#O..O"
    ".......O.."
    "#....###.."
    "#OO..#...."))

;; I don't know if these are useful
(defn rows [grid]
  (group-by second (grid/coords grid)))

(defn columns [grid]
  (group-by first (grid/coords grid)))

(columns (grid/parse example-grid))
;; seems like the best approach is to operate on the grid rows/columns
;; individually and then put them back in
;; we'll just implement a basic slide function.

(defn slide [v]
  (loop [v v
         x 0]
    (if-let [h (get v x)]
      (case h
        \# (recur v (inc x))
        \O (recur v (inc x))
        \. ;; walk forward to find y
        (if-let [y (->> (range (inc x) (count v))
                        (drop-while #(= (get v %) \.))
                        (first))]
          (case (get v y)
            \# (recur v (inc y))
            \O (recur
                (-> v
                    (assoc x \O)
                    (assoc y \.))
                (inc x)))
          v))
      v)))

(slide (vec (seq "#O#...O...O..#..O..#")))

(defn get-row [grid y]
  (grid y))

(defn get-column [grid x]
  (let [[_ ymax] (grid/bounds grid)]
    (mapv (partial grid/at grid) (for [y (range 0 ymax)] [x y]))))

(get-column (grid/parse example-grid) 0)

;; rows are just updating the grid's structure as it exists now, so it's easy
(defn replace-row [grid y new-row]
  (assoc grid y (vec new-row)))

;; columns are a bit annoying, but a nice thing here is that this takes
;; sequences
(defn replace-column  [grid x new-column]
  (let [[_ ymax] (grid/bounds grid)]
    (reduce (fn [grid [[nx ny] new-ch]]
              (assoc-in grid [ny nx] new-ch))
            grid
            (map vector
                 (for [y (range 0 ymax)] [x y])
                 new-column))))

(defn slide-direction [grid direction]
  (let [[xmax ymax] (grid/bounds grid)]
    (case direction
      :up
      (reduce
       (fn [grid x]
         (replace-column grid x (slide (get-column grid x))))
       grid
       (range 0 xmax))
      :left
      (reduce
       (fn [grid y]
         (replace-row grid y (slide (get-row grid y))))
       grid
       (range 0 ymax))
      :right
      (reduce
       (fn [grid y]
         (replace-row grid y (rseq (slide (vec (rseq (get-row grid y)))))))
       grid
       (range 0 ymax))
       :down
       (reduce
        (fn [grid x]
          (replace-column grid x (rseq (slide (vec (rseq (get-column grid x)))))))
        grid
        (range 0 xmax)))))

(slide-direction (grid/parse example-grid) :left)

(defn total-load [grid]
  (let [[_ ymax] (grid/bounds grid)]
    (->> grid
         (map-indexed
          (fn [n row]
            (* (- ymax n) (count (filter (partial = \O) row)))))
         (reduce +))))

;; correct
(total-load (slide-direction (grid/parse (utils/read-input "2023/day14.txt")) :up))
