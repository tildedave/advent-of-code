(ns advent2019.day11
  (:require [advent2019.intcode :as intcode]
            [clojure.string :as string]
            [clojure.core.async :as a :refer [<!! >! <! >!!]]
            [clojure.core.match :refer [match]]
            [utils :as utils]))

(defn next-direction [direction turn-direction]
  (match [direction turn-direction]
    [:up 0] :left
    [:up 1] :right
    [:left 0] :down
    [:left 1] :up
    [:down 0] :right
    [:down 1] :left
    [:right 0] :up
    [:right 1] :down))

(defn walk-from [[x y] direction]
  (let [[dx dy]
        (case direction
          :up [0 -1]
          :down [0 1]
          :left [-1 0]
          :right [1 0])]
    [(+ x dx) (+ y dy)]))

(defn run-robot [part2?]
  (let [input (a/chan)
        output (intcode/run-file "2019/day11.txt" input)]
    (a/go-loop [white-tiles (if part2? #{[0 0]} #{})
                all-tiles #{}
                position [0 0]
                direction :up]
      (a/put! input (if (contains? white-tiles position)
                      1
                      0))
      (let [panel-color (<! output)
            turn-direction (<! output)]
        (if (nil? panel-color)
          [all-tiles white-tiles]
          (let [direction (next-direction direction turn-direction)]
            (recur
             (if (zero? panel-color)
               (disj white-tiles position)
               (conj white-tiles position))
             (if (zero? panel-color)
               all-tiles
               (conj all-tiles position))
             (walk-from position direction)
             direction)))))))

(defn answer-part1 []
  (count (first (<!! (run-robot false)))))

(println (answer-part1))

(defn answer-part2 []
  (let [painted-tiles (second (<!! (run-robot true)))
        xmin (reduce min (map first painted-tiles))
        xmax (reduce max (map first painted-tiles))
        ymin (reduce min (map second painted-tiles))
        ymax (reduce max (map second painted-tiles))]
    (string/join
     "\n"
     (for [y (utils/range-inclusive ymin ymax)]
       (string/join
        (for [x (utils/range-inclusive xmin xmax)]
          (if (contains? painted-tiles [x y])
            \# \.)))))))

(println (answer-part2))
