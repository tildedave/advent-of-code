(ns advent2022.day18
  (:require [advent2022.utils :as utils]
            [clojure.math.combinatorics :as combo]))

(defn parse-line [str]
  (mapv utils/parse-int (.split str ",")))

(def lines (utils/read-resource-lines "input/day18.txt"))

(defn cube-vertices [[x y z]]
  (for [x [x (inc x)]
        y [y (inc y)]
        z [z (inc z)]]
    [x y z]))

(defn compare-vertices [[x0 & xs0] [x1 & xs1]]
  (cond (= x0 x1) (compare-vertices xs0 xs1)
        :else (compare x0 x1)))

(use 'clojure.tools.trace)
(defn cube-faces [[x y z]]
  (let [vertices (cube-vertices [x y z])]
    (->> (combo/combinations vertices 4)
         (filter
          (fn [vs]
            (some true? (map (fn [x] (apply = x))
                             (for [n (range 0 3)]
                               (map (fn [v] (nth v n)) vs))))))

         (map (partial sort compare-vertices)))))

(count (cube-faces [1 1 2]))

(defn count-faces [acc cube]
  (->>
   (cube-faces cube)
   (reduce
    (fn [acc face]
      (-> acc
          (update-in [face] (fnil inc 0))))
    acc)))

(defn surface-area [cubes]
  (->> cubes
       (reduce count-faces {})
       (vals)
       (filter #(= % 1))
       (count)))

(surface-area (map parse-line lines))
