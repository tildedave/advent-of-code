(ns advent2015.day4
  (:require [utils :as utils]))

(def ^:dynamic part2? false)

(defn answer [prefix]
  (let [md (java.security.MessageDigest/getInstance "MD5")]
    (loop [suffix 0]
      (let [[n1 n2 n3] (->> (format "%s%d" prefix suffix)
                            (.getBytes)
                            (.digest md)
                            (take 3))]
        (if (and (zero? n1) (zero? n2) (zero? (bit-and n3 (if part2? 0xF0 0xFF))))
                 suffix
                 (recur (inc suffix)))))))

(def answer-part1 answer)
(def answer-part2 (binding [part2? true] answer))

(answer-part1 (first (utils/read-input "2015/day4.txt")))
(answer-part2 (first (utils/read-input "2015/day4.txt")))
