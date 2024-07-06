(ns advent2019.day24
  (:require [grid :as grid]
            [clojure.set :as set]
            [clojure.string :as string]))

;; well this one is just a straightforward game of life
;; part 2 is a bit more of a curveball, but whatever.
;; for now, we code part 1.

(defn initial-spaces [grid]
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
  (print-grid-part1 (tick (neighbors-part1 grid) (initial-spaces grid))))

;; OK, so part 1 is easy.

(defn biodiversity-rating [[x y]]
  (bit-shift-left 1 (+ (* y 5) x)))

(defn answer-part1 [grid]
  (->> (initial-spaces grid)
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
