(ns advent2015.day18
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [grid :as grid]
            [utils :as utils]))

(def ^:dynamic part2? false)

(defn neighbors [[xmax ymax]]
  (memoize (fn [[x y]]
             (->> (for [dx [-1 0 1]
                        dy [-1 0 1]]
                    (let [[nx ny] [(+ x dx) (+ y dy)]]
                      (cond
                        (= dx dy 0) nil
                        (>= nx xmax) nil
                        (>= ny ymax) nil
                        (< nx 0) nil
                        (< ny 0) nil
                        :else [nx ny])))
                  (remove nil?)
                  (set)))))

(defn step [bounds neighbors light-coords]
  ;; for each light, find its neighbors, count how many are on
  (let [turn-off (->>
                  (for [coord light-coords]
                    (if
                     (<= 2
                         (count (set/intersection (neighbors coord) light-coords))
                         3)
                      nil
                      coord))
                  (remove nil?)
                  (set))
        turn-on (->>
                 (for [coord light-coords]
                   (->> (set/difference (neighbors coord) light-coords)
                        (map #(hash-map % 1))))
                 (apply concat)
                 (apply merge-with +)
                 (remove (fn [[_ n]] (not= n 3)))
                 (map first)
                 (set))]
    (let [next (-> light-coords
        (set/difference turn-off)
        (set/union turn-on))]
      (if part2?
        (let [[xmax ymax] bounds]
          (-> next
              (conj [0 0])
              (conj [0 (dec ymax)])
              (conj [(dec xmax) 0])
              (conj [(dec xmax) (dec ymax)])))
        next))))


(defn light-seq [filename]
  (let [grid (grid/parse (utils/read-input (format "2015/%s" filename)))
        bounds (grid/bounds grid)
        neighbors (neighbors bounds)
        initial-coords (->> (grid/coords grid)
                            (filter #(= (grid/at grid %) \#))
                            (set))]
    (iterate (partial step bounds neighbors) initial-coords)
    ))

(defn print-grid [[xmax ymax] light-coords]
  (println
   (string/join "\n" (for [y (range ymax)]
    (string/join
     (for [x (range xmax)]
      (if (contains? light-coords [x y])
        \#
        \.)))))))

(defn answer [filename n]
  (count (nth (light-seq filename) n)))


(assert (= (answer "day18-example.txt" 4) 4))
(answer "day18.txt" 100)

;; (print-grid [6 6] (nth (light-seq "day18-example.txt") 4))

  ;; for each light, find its neighbors, a neighbor that is NOT a light but
  ;; who shows up 3 times, becomes on.
