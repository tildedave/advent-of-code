(ns advent2015.day11
  (:require [clojure.string :as string]
            [utils :as utils]))

(def letter-seq (seq "abcdefghijklmnopqrstuvwxyz"))

(def next-letter
  (->> (map (fn [a b] {a b})
            letter-seq
            (rest letter-seq))
       (reduce merge)))

(defn increment [^String password]
  (loop [password password
          n (dec (.length password))]
         (if (< n 0) "#ERROR"
             (let [ch (.charAt password n )
                   next-ch (get next-letter ch \a)
                   next-password (string/join
                                  [(.substring password 0 n)
                                   next-ch
                                   (.substring password (inc n) (.length password))])]
               (case ch
                 \z (recur
                     next-password
                     (dec n))
                 next-password)))))

(defn has-increasing-straight? [^String str]
  (loop [n 0]
  (if (>= (+ n 2) (.length str))
    false
    (let [ch1 (.charAt str n)
          ch2 (.charAt str (inc n))
          ch3 (.charAt str (+ n 2))]
      (if (and (= (next-letter ch1) ch2)
               (= (next-letter ch2) ch3))
        true
        (recur (inc n)))))))

(defn has-illegal-vowel? [^String str]
  (loop [n 0]
    (if (= n (.length str))
      false
      (let [ch (.charAt str n)]
        (case ch
          (\i \o \l) true
          (recur (inc n)))))))

(defn num-duplicates [^String str]
  (loop [n 0
         total 0]
    (if (>= n (dec (.length str)))
      total
      (let [ch1 (.charAt str n)
            ch2 (.charAt str (inc n))]
        (if (= ch1 ch2)
          (recur (+ n 2) (inc total))
          (recur (inc n) total))))))

(defn is-legal-password? [password]
  (and (not (has-illegal-vowel? password))
       (>= (num-duplicates password) 2)
       (has-increasing-straight? password)))

(defn next-password [password]
  (->> (iterate increment (increment password))
       (filter is-legal-password?)
       (first)))

(defn answer-part1 [filename]
  (->> (format "2015/%s" filename)
       (utils/read-input)
       (first)
       (next-password)))

(defn answer-part2 [filename]
  (->> (format "2015/%s" filename)
       (utils/read-input)
       (first)
       (next-password)
       (next-password)))

(answer-part1 "day11.txt")
(answer-part2 "day11.txt")
