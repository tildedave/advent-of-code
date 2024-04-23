(ns advent2016.day3
  (:require [utils :as utils]
            [clojure.string :as string]))

(defn is-triangle-valid? [sides]
  (let [[x y z] (sort sides)]
    (> (+ x y) z)))

(defn answer-part1 []
  (->> (utils/read-input "2016/day3.txt")
       (map #(rest (re-matches #"^\s+(\d+)\s+(\d+)\s+(\d+)\s*$" %)))
       (map #(map utils/parse-int %))
       (filter is-triangle-valid?)
       (count)))

(string/split (string/trim "  785  516  744") #"\s+")

(defn answer-part2 []
  (->> (utils/read-input "2016/day3.txt")
       (map #(map utils/parse-int
                   (-> % (string/trim) (string/split #"\s+"))))
       (apply interleave)
       (partition 3)
       (filter is-triangle-valid?)
       (count)
       ))

(answer-part2)
