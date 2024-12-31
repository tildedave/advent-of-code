(ns advent2017.day4
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]))

(set! *warn-on-reflection* true)

(defn is-valid? [^String passphrase]
   (->> (.split passphrase " ")
        (map #(hash-map % 1))
        (apply merge-with +)
        (remove (fn [[_ n]] (= n 1)))
        (empty?)))

(is-valid? "aa bb cc dd aaa")

(defn answer-part1 []
  (->> (utils/read-input "2017/day4.txt")
     (filter is-valid?)
     (count)))

(defn is-anagram? [^String s1 ^String s2]
  (and (= (.length s1) (.length s2))
       (= (reduce (partial merge-with +) (map #(hash-map % 1) (seq s1)))
          (reduce (partial merge-with +) (map #(hash-map % 1) (seq s2))))))

(is-anagram? "abcde" "ecdab")

(defn answer-part2 []
  (->> (utils/read-input "2017/day4.txt")
       (filter (fn [s] (as-> (.split s " ") x
                             (combo/combinations x 2)
                             (filter #(apply is-anagram? %) x)
                             (empty? x))))
       (count)))

(answer-part2)
