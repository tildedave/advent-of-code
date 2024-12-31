(ns advent2020.day17
  (:require [utils :as utils]
            [grid :as grid]
            [clojure.math.combinatorics :as combo]))

(def ^:dynamic part2? false)

(defn parse-active-cubes [filename]
  (let [grid-lines (grid/parse (utils/read-input (format "2020/%s" filename)))]
    (->> (grid/coords grid-lines)
                  (filter #(= (grid/at grid-lines %) \#))
                  (map #(into % (if part2? [0 0] [0])))
                  (set))))

(defn neighbors [cube]
  (->>
   (for [delta (apply combo/cartesian-product (repeat (count cube) [-1 0 1]))]
     (map + delta cube))
   (remove (partial = cube))))

(defn inactivate-cubes [active-cubes]
  (filter
   (fn [cube]
     (let [active-neighbors (filter (partial contains? active-cubes) (neighbors cube))]
       (case (count active-neighbors)
         2 false
         3 false
         true)))
   active-cubes))

(defn activate-cubes [active-cubes]
  (->> (map neighbors active-cubes)
       (apply concat)
       (map (fn [x] {x 1}))
       (apply merge-with (fnil + 0))
       (filter #(= (second %) 3))
       (map first)
       (remove #(contains? active-cubes %))
       ))

(activate-cubes (parse-active-cubes "day17-example.txt"))


(defn step [active-cubes]
  (reduce
   conj
   (reduce disj active-cubes (inactivate-cubes active-cubes))
   (activate-cubes active-cubes)))

(step (parse-active-cubes "day17-example.txt"))

(activate-cubes #{[1 0 0] [2 2 0] [0 2 0] [1 2 0] [2 1 0]})

(defn answer [filename]
  (->> (parse-active-cubes filename)
       (iterate step)
       (take 7)
       (last)
       (count)))

(defn answer-part1 [filename] (answer filename))
(answer-part1 "day17-example.txt")
(answer-part1 "day17.txt")

(defn answer-part2 [filename] (binding [part2? true] (answer filename)))
(answer-part2 "day17-example.txt")
(answer-part2 "day17.txt")
