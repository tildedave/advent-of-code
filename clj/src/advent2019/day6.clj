(ns advent2019.day6
  (:require [grid :as grid]
            [utils :as utils]
            [clojure.set :as set]))

;; I suppose this is easiest to do using DFS.

(defn parse-orbit [^String line]
  (apply hash-map (reverse (.split line "\\)"))))

(defn parse-orbits [lines]
  (reduce merge {} (map parse-orbit lines)))

(parse-orbits (utils/read-input "2019/day6-example.txt"))

;; memoized recursive function
(defn num-orbits [parents]
  (let [num-orders-rec (fn [rec n]
                            (if-let [parent (parents n)]
                              (inc (rec rec parent))
                              0))
        memoized-rec (memoize num-orders-rec)]
    (partial memoized-rec memoized-rec)))

;; this is obviously a bit of a sledgehammer.
;; we could topo sort this to get the answer quicker.
;; yes we do need to topo sort.
(defn answer-part1 [lines]
  (let [orbits (parse-orbits lines)
        num-orbits (num-orbits orbits)]
    (->> (keys orbits)
         (map num-orbits)
         (reduce + 0))))

(answer-part1 (utils/read-input "2019/day6-example.txt"))
(answer-part1 (utils/read-input "2019/day6.txt"))

(defn all-orbits [parents]
   (let [all-orbits-rec (fn [rec n]
                          (if-let [parent (parents n)]
                            (conj (rec rec parent) n)
                            [n]))
         memoized-rec (memoize all-orbits-rec)]
     (partial memoized-rec memoized-rec)))

((all-orbits (parse-orbits (utils/read-input "2019/day6-example2.txt"))) "YOU")
((all-orbits (parse-orbits (utils/read-input "2019/day6-example2.txt"))) "SAN")

(defn answer-part2 [lines]
  (let [orbits (parse-orbits lines)
        all-orbits (all-orbits orbits)]
    (loop [orbits-you (all-orbits "YOU")
           orbits-san (all-orbits "SAN")]
      (if (= (first orbits-you)
             (first orbits-san))
        (recur (subvec orbits-you 1)
               (subvec orbits-san 1))
        (+ (count orbits-you) (count orbits-san) -2)))))

(answer-part2 (utils/read-input "2019/day6-example2.txt"))
(answer-part2 (utils/read-input "2019/day6.txt"))

