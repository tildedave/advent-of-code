(ns advent2019.day16
  (:require [utils :as utils]
            [clojure.string :as string]))

(def base-pattern [0 1 0 -1])
(def pattern-at-idx
  (memoize (fn [n]
           (rest (mapcat
                  (partial repeat n)
                  (cycle base-pattern))))))

(defn parse-input [s]
  (map #(utils/parse-int (str %)) (seq s)))

(defn apply-phase [input-signal]
  (->> (range 1 (inc (count input-signal)))
       (map (fn [n]
              (->> (map * input-signal (pattern-at-idx n))
                   (reduce +)
                   (abs)
                   (#(mod % 10)))))))

;; part 1
(defn answer-part1 [line]
  (as-> (parse-input line) a
    (iterate apply-phase a)
    (nth a 100)
    (take 8 a)
    (string/join a)))

(answer-part1 "80871224585914546619083218645595")
(answer-part1 "19617804207202209144916044189917")
(answer-part1 "69317163492948606335995924319873")
(answer-part1 "69317163492948606335995924319873")
(answer-part1 (utils/read-input-line "2019/day16.txt"))

;; so part 2 needs to be sneakier
