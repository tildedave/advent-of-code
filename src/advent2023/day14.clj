(ns advent2023.day14
  (:require [grid :as grid]))

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


(defn slide-direction [grid direction]
  (let [[xmax ymax] (grid/bounds grid)
        rows (rows grid)
        columns (columns grid)]
    (->>
     (case direction
       :up
       (for [x (range 0 xmax)]
          ;; these are the coordinates, it seems they show up sorted
          ;; we do need to manually sort since certain directions involve
          ;; approaching from a different direction
          ;; e.g. going down requires sorting by >
         (->> (columns x)
              (sort-by second)))
       :down
       (for [x (range 0 xmax)]
         (->> (columns x)
              (sort-by second >)))
       (throw (Exception. "not implemented")))
     (map (fn [coords]
            (loop
             [coords coords
              result {}]
              (if-let [x (first coords)]
                (case (grid/at grid x)
                  \O (recur (rest coords) result) ;; no change for this one, go to the next
                  \# (recur (rest coords) result) ;; also no change
                  \. ;; two possibilities: there is a next rock and there is no next rock
                  (let [find-rock-coords (drop-while #(= (grid/at grid %) \.) (rest coords))]
                    (if-let [y (first find-rock-coords)]
                      (case (grid/at grid y)
                        \O (recur (rest coords) (assoc result y x))
                        \# (recur (rest coords) result))
                      result))
                  (throw (Exception. (format "unrecognized %s %s" (str (grid/at grid x)) x))))
                result))))
     (reduce merge {})
    ;;  (reduce
    ;;   (fn [grid [[old new]]]
    ;;     (grid/assoc ))
    ;;   grid))))
    )))

(slide-direction (grid/parse example-grid) :up)
