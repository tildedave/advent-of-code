(ns advent2018.day18
  (:require [grid :as grid]
            [clojure.set :as set]
            [clojure.string :as string]
            [utils :as utils]
            [clojure.walk :refer [walk]]))

(defn neighbors [[xmin xmax] [ymin ymax]]
  (memoize
   (fn [[x y]]
     (->>
      (for [[dx dy] grid/all-directions]
        [(+ x dx) (+ y dy)])
      (filter (fn [[x y]]
                (and (<= xmin x xmax)
                     (<= ymin y ymax))))
      (set)))))

(defn neighbor-3set [neighbors s]
  (->> s
       (mapcat neighbors)
       (frequencies)
       (filter (fn [[_ n]] (>= n 3)))
       (map first)
       (set)))

(defn tick [neighbors state]
  (let [{:keys [trees lumberyards]} state
        new-trees (set/difference
                   (neighbor-3set neighbors trees)
                   trees
                   lumberyards)
        new-lumberyards (set/intersection
                         trees
                         (set/difference
                          (neighbor-3set neighbors lumberyards)))
        remaining-lumberyards (->> lumberyards
                                   (remove
                                    (fn [lcoords]
                                      (let [nset (neighbors lcoords)]
                                        (or (empty? (set/intersection nset trees))
                                            (empty? (set/intersection nset lumberyards))))))
                                   (set))]
    {:trees (-> trees
                (set/difference new-lumberyards)
                (set/union new-trees))
     :lumberyards (set/union new-lumberyards remaining-lumberyards)}))

(defn print-grid [{:keys [trees lumberyards]}]
  (let [all-coords (set/union trees lumberyards)
        max-x (walk first (partial reduce max) all-coords)
        min-x (walk first (partial reduce min) all-coords)
        min-y (walk second (partial reduce min) all-coords)
        max-y (walk second (partial reduce max) all-coords)]
    (string/join
     "\n"
     (for [y (range min-y (inc max-y))]
       (string/join
        (for [x (range min-x (inc max-x))]
          (cond
            (contains? trees [x y]) \|
          (contains? lumberyards [x y]) \#
            :else \.)))))))

(defn parse-grid [filename]
  (let [grid (grid/parse (utils/read-input filename))]
    [(as-> (grid/coords grid) s
      (group-by (fn [coord] (case
                             (grid/at grid coord)
                              \. :open
                              \# :lumberyards
                              \| :trees)) s)
      (dissoc s :open)
      (update-vals s set))
     (grid/bounds grid)]
     ))

(defn answer-part1 [filename]
  (let [[grid [xmax ymax]] (parse-grid filename)
        _ (println xmax ymax)]
    (reduce
     *
     (-> (iterate (partial tick (neighbors [0 (dec xmax)] [0 (dec ymax)])) grid)
         (nth 10)
         (update-vals count)
         (vals)))))

(answer-part1 "2018/day18.txt")

(-> (iterate (partial tick (neighbors [0 9] [0 9])) (parse-grid "2018/day18-example.txt"))
    (nth 10)
    (update-vals count)
    (vals))
