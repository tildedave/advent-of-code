(ns advent2019.day24
  (:require [grid :as grid]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.core.match :refer [match]]))

;; well this one is just a straightforward game of life
;; part 2 is a bit more of a curveball, but whatever.
;; for now, we code part 1.

(defn initial-spaces-part1 [grid]
  (->> (grid/coords grid)
       (filter #(= (grid/at grid %) \#))
       (set)))

(defn neighbors-part1 [grid]
  (fn [[x y]]
    (grid/neighbors grid [x y])))

;; bug-spaces is a set
(defn tick [neighbors bug-spaces]
  ;; bugs need neighbors
  ;; for each bug space, get its neighbors
  (let [bug-neighbors (into {} (map (fn [c] {c (set (neighbors c))}) bug-spaces))
        dead-bugs (->> bug-spaces
                       (filter (fn [c]
                                 (not= (count
                                        (set/intersection bug-spaces (bug-neighbors c)))
                                       1))))
        ;; for all bugs, find their neighbors, anything that shows more than once
        ;; (and is not a bug itself) is a
        new-bugs (->> (map frequencies (vals bug-neighbors))
                      (reduce (partial merge-with +) {})
                      (filter (fn [[c n]]
                                (and (not (contains? bug-spaces c))
                                     (or (= n 1) (= n 2)))))
                      (map first)
                      (set))]
    (-> bug-spaces
        (set/difference dead-bugs)
        (set/union new-bugs))))

;; this is a 5x5 grid
(defn print-grid-part1 [bug-spaces]
  (println (string/join
            "\n"
            (for [y (range 0 5)]
              (string/join
               (for [x (range 0 5)]
                 (if (contains? bug-spaces [x y])
                   \#
                   \.)))))))

(let [grid  (grid/parse
             '("....#"
               "#..#."
               "#..##"
               "..#.."
               "#...."))]
;;   ((neighbors-part1 grid) [0 0]))
  (print-grid-part1 (tick (neighbors-part1 grid) (initial-spaces-part1 grid))))

;; OK, so part 1 is easy.

(defn biodiversity-rating [[x y]]
  (bit-shift-left 1 (+ (* y 5) x)))

(defn answer-part1 [grid]
  (->> (initial-spaces-part1 grid)
       (iterate
        (partial tick (neighbors-part1 grid)))
       (map #(reduce + (map biodiversity-rating %)))
       (reduce (fn [acc rating]
                 (if (contains? acc rating)
                   (reduced rating)
                   (conj acc rating)))
               #{})))

(answer-part1
 (grid/parse
  '("....#"
    "#..#."
    "#..##"
    "..#.."
    "#....")))

(answer-part1
 (grid/parse-file "2019/day24.txt"))

;; OK so this next part is more annoying.
;; my "tick" function will work the same.
;; I just need to beef up the "neighbors" logic.

(defn initial-spaces-part2 [grid]
  (->> (grid/coords grid)
       (filter #(= (grid/at grid %) \#))
       (map (fn [c] {:coords c :depth 0}))
       (set)))

(initial-spaces-part2 (grid/parse
                       '("....#"
                         "#..#."
                         "#..##"
                         "..#.."
                         "#....")))

;; depth ABOVE means stuff outside our current grid.
;; depth BELOW means stuff inside our current grid.
(defn neighbors-part2 [{:keys [coords depth]}]
  (mapcat
   (fn [dcoords]
     (let [[nx ny] (mapv + coords dcoords)]

       (match [nx ny]
                     ;; above the example grid
         [_ -1] [{:coords [2 1] :depth (inc depth)}]
                     ;; to the right of the example grid
         [-1 _] [{:coords [1 2] :depth (inc depth)}]
                     ;; below the example grid
         [_ 5] [{:coords [2 3] :depth (inc depth)}]
                     ;; to the left of the example grid
         [5 _] [{:coords [3 2] :depth (inc depth)}]
                     ;; the middle of the example grid.
                     ;; in this case, the neighbors matter based on
                     ;; our current dcoords.
         [2 2] (case dcoords
                             ;; from the top
                 [0 1] (for [x (range 0 5)]
                         {:coords [x 0] :depth (dec depth)})
                             ;; from the bottom
                 [0 -1] (for [x (range 0 5)]
                          {:coords [x 4] :depth (dec depth)})
                             ;; from the left
                 [1 0] (for [y (range 0 5)]
                         {:coords [0 y] :depth (dec depth)})
                             ;; from the right
                 [-1 0] (for [y (range 0 5)]
                          {:coords [4 y] :depth (dec depth)}))
         [_ _] [{:coords [nx ny] :depth depth}])))

   '([0 1] [0 -1] [-1 0] [1 0])))

(neighbors-part2 {:coords [2 3] :depth 0})

(let [grid  (grid/parse
             '("....#"
               "#..#."
               "#..##"
               "..#.."
               "#...."))]
  (->>
   (iterate
    (partial tick (memoize neighbors-part2))
    (initial-spaces-part2 grid))
   (drop 10)
   (first)
   (count)))

(defn answer-part2 [grid n]
  (->> grid
       (initial-spaces-part2)
       (iterate (partial tick (memoize neighbors-part2)))
       (drop n)
       (first)
       (count)))

(answer-part2 (grid/parse
               '("....#"
                 "#..#."
                 "#..##"
                 "..#.."
                 "#....")) 10)

(answer-part2
 (grid/parse-file "2019/day24.txt")
 200)
