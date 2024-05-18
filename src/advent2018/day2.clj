(ns advent2018.day2
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as string]))

(defn two-or-three [s]
  (let [s (->> (frequencies s) (vals) (set))]
    (concat
     (if (contains? s 2) '(2) '())
     (if (contains? s 3) '(3) '())
     '())))

    ;;    (vals)
    ;;    (filter #(or (= % 2) (= % 3)))
    ;;    (first)))

(two-or-three "abccdeee")

(->> (utils/read-input "2018/day2.txt")
     (map two-or-three)
     (flatten)
     (remove nil?)
     (sort)
     (partition-by identity)
     (map count)
     (reduce *))

(->> (combo/combinations (utils/read-input "2018/day2.txt") 2)
     (map (fn [[s1 s2]]
            (let [diffs (->> (map = (seq s1) (seq s2))
                             (remove true?))]
              (if (= (count diffs) 1)
                (->> (map vector (seq s1) (seq s2))
                     (filter #(apply = %))
                     (map first)
                     (string/join)
                     )
                nil))))
     (remove nil?)
     (first))
