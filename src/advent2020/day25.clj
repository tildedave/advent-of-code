(ns advent2020.day25
  (:require [utils :as utils]))

;; this is just solving the discrete logarithm problem

(defn step [subject-number result]
  (mod (* result subject-number) 20201227))

(defn discrete-log [public-key]
  (->>
   (iterate (partial step 7) 1)
   (map-indexed
    (fn [n x] (if (= x public-key) n nil)))
   (remove nil?)
   (first)))

(discrete-log 5764801)
(discrete-log 17807724)

(defn answer-part1 [door-public-key card-public-key]
  (nth (iterate (partial step door-public-key) 1) (discrete-log card-public-key)))

(answer-part1 17807724 5764801)

(->> (utils/read-input "2020/day25.txt")
     (map utils/parse-int)
     (apply answer-part1))
