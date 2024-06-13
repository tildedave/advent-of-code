(ns advent2019.day4
  (:require [utils :as utils]))

(defn to-digits [n left]
  (if (zero? left)
    []
    (conj (to-digits (quot n 10) (dec left)) (mod n 10))))

(defn increasing? [digits]
  (let [res (reduce (fn [prev curr]
            (if (< curr prev)
              (reduced false)
              curr))
          -1
          digits)]
  (if (number? res) true res)))

(defn matches-part1? [digits]
  (and (increasing? digits)
       (->> digits
            (partition-by identity)
            (map count)
            (set)
            (remove (partial < 2))
            (seq))))

;; answer part 1
(->> (.split (first (utils/read-input "2019/day4.txt")) "-")
     (map utils/parse-int)
     (apply utils/range-inclusive)
     (map #(to-digits % 6))
     (filter matches-part1?)
     (count))

;; answer part 2
(->> (.split (first (utils/read-input "2019/day4.txt")) "-")
     (map utils/parse-int)
     (apply utils/range-inclusive)
     (map #(to-digits % 6))
     (filter matches-part1?)
     (filter #(->> %
                   (partition-by identity)
                   (map count)
                   (set)
                   (filter (partial = 2))
                   (seq)))
     (count))
