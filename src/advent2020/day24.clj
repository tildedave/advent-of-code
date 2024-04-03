(ns advent2020.day24
  (:require [utils :as utils]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]))

(defn parse-line [line]
  (loop [line-seq (seq line)
         result []]
    (if (empty? line-seq)
      result
      (let [[next next-seq] (match (vec (take 2 line-seq))
                              [\e] [:east '()]
                              [\w] [:west '()]
                              [\w _] [:west (rest line-seq)]
                              [\e _] [:east (rest line-seq)]
                              [\n \e] [:north-east (drop 2 line-seq)]
                              [\n \w] [:north-west (drop 2 line-seq)]
                              [\s \e] [:south-east (drop 2 line-seq)]
                              [\s \w] [:south-west (drop 2 line-seq)])]
        (recur next-seq (conj result next))))))

(parse-line "sesenwnenenewseeswwswswwnenewsewsw")

(defn walk [[x y] step]
  (case step
    :west [(- x 2) y]
    :east [(+ x 2) y]
    :north-east [(inc x) (inc y)]
    :north-west [(dec x) (inc y)]
    :south-east [(inc x) (dec y)]
    :south-west [(dec x) (dec y)]))

(assert (= (reduce walk [0 0] (parse-line "nwwswee")) [0 0]))

(defn flip-tile [black-tile-set steps]
  (let [[dx dy] (reduce walk [0 0] steps)]
    (if (contains? black-tile-set [dx dy])
      (disj black-tile-set [dx dy])
      (conj black-tile-set [dx dy]))))

(defn starting-tiles [filename]
  (reduce
   flip-tile
   #{}
   (->> filename
        (format "2020/%s")
        (utils/read-input)
        (map parse-line))))

(defn answer-part1 [filename]
  (->
   (starting-tiles filename)
   (count)))

(answer-part1 "day24-example.txt")
(answer-part1 "day24.txt")

(defn neighbor-tiles [[tx ty]]
  #{[(+ tx 2) ty]
        [(- tx 2) ty]
        [(inc tx) (inc ty)]
        [(inc tx) (dec ty)]
        [(dec tx) (inc tx)]
        [(dec tx) (dec tx)]})

(starting-tiles "day24-example.txt")

(defn step-tiles [black-tile-set]
  (let [new-white-tiles (->>
                         (for [t black-tile-set]
                          (let [black-neighbors (-> (neighbor-tiles t)
                                                    (set/intersection black-tile-set))
                                n (count black-neighbors)]
                            (if (or (= n 0) (> n 2)) t nil)))
                         (remove nil?)
                         (set))
        new-black-tiles (->> black-tile-set
                             (map #(map (fn [t] {t 1})
                                        (-> (neighbor-tiles %)
                                            (set/difference black-tile-set))))
                             (flatten)
                             (reduce (partial merge-with +) {})
                             (filter #(= (second %) 2))
                             (map first)
                             (set))]
    (reduce conj
            (reduce disj black-tile-set new-white-tiles)
            new-black-tiles)))

(take 4 (map count (iterate step-tiles (starting-tiles "day24-example.txt"))))

(step-tiles #{[0 0] [2 2]})

    (neighbor-tiles x)
