(ns advent2016.day7
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]))


(defn has-abba? [s]
  (if (< (count s) 4)
    false
    (let [[ch1 ch2 ch3 ch4] (seq s)]
      (if (and (= ch1 ch4)
           (= ch2 ch3)
           (not= ch1 ch2))
        true
        (recur (.substring s 1 (count s)))))))

(has-abba? "abba")

(defn split-on-brackets [s]
  (loop [s s
         results1 []
         results2 []]
  (let [[m out-of-brackets in-brackets rest] (re-matches #"(\w+)\[(\w+)\](.*)"s)]
    (if m
      (recur rest (conj results1 out-of-brackets) (conj results2 in-brackets))
      [(conj results1 s) results2]))))

(split-on-brackets "abba[mnop]qrst")

(defn has-tls? [s]
  (let [[in-brackets out-of-brackets] (split-on-brackets s)]
    (and
     (not-any? has-abba? out-of-brackets)
     (>= (count (filter has-abba? in-brackets)) 1))))

(has-tls? "abba[mnop]qrst")
(has-tls? "abcd[bddb]xyyx")
(has-tls? "aaaa[qwer]tyui")
(has-tls? "ioxxoj[asdfgh]zxcvbn")

(defn answer-part1 [lines]
  (count (filter has-tls? lines)))

(answer-part1 (utils/read-input "2016/day7.txt"))

(defn abas [s]
  (loop [s s
         result []]
    (if (< (count s) 3)
      result
      (let [[ch1 ch2 ch3] (seq s)]
        (if (and (= ch1 ch3)
                 (not= ch1 ch2))
          (recur (.substring s 1 (count s))
                 (conj result (str ch1 ch2 ch3)))
          (recur (.substring s 1 (count s))
                 result))))))

(defn has-aba-bab-match? [[s1 s2]]
  (let [[[ch1 ch2 ch3] [ch1' ch2' ch3']] [(seq s1) (seq s2)]]
    (and (= ch1 ch2' ch3)
         (= ch1' ch2 ch3'))))



(defn has-ssl? [s]
  (let [[in out] (split-on-brackets s)]
    (->>
     (combo/cartesian-product
      (flatten (mapv abas in))
      (flatten (mapv abas out)))
     (filter has-aba-bab-match?)
     (count)
     (not= 0))))

(has-ssl? "zazbz[bzb]cdb")
(has-ssl? "xyx[xyx]xyx")
(has-ssl? "aaa[kek]eke")
(has-ssl? "aba[bab]xyz")


(defn answer-part2 [lines]
  (count (filter has-ssl? lines)))

(answer-part2 (utils/read-input "2016/day7.txt"))
