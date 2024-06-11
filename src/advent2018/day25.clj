(ns advent2018.day25
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as string]
            [clojure.set :as set]))

(def distance (memoize utils/manhattan-distance))

(defn in-range? [constellation point]
  (not (empty? (filter #(<= (utils/manhattan-distance point %) 3)
                       constellation))))

(defn parse-point [^String s]
  (->> (.split (string/trim s) ",")
       (map utils/parse-int)))

(defn answer [filename]
  (->> (utils/read-input filename)
       (map parse-point)
       (reduce
        (fn [constellations point]
          (let [constellation-matches (group-by #(in-range? % point) constellations)]
            (if (empty? (constellation-matches true))
              (conj constellations #{point})
              (conj (constellation-matches false)
                    (reduce set/union #{point} (constellation-matches true))))))
        '())
       (count)))

(answer "2018/day25-example1.txt")
(answer "2018/day25-example2.txt")
(answer "2018/day25-example3.txt")
(answer "2018/day25-example4.txt")
(answer "2018/day25.txt")
