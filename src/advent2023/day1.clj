(ns advent2023.day1
  (:require [utils :as utils]))

;; OK, so first number from the left, first number from the right
;; I happen to know that part 2 will ask for it to handle strings
;; like "one", "two", etc.
;; we want to run it through a lot of possibilities.

(defn digit-value [acc ch]
  (case ch
    (\0  \1 \2 \3 \4 \5 \6 \7 \8 \9) (reduced (utils/parse-int (str ch)))
    nil))

;; I would like to write this function
(defn calibration-value [line]
  (+ (* 10 (reduce digit-value nil (seq line)))
     (reduce digit-value nil (reverse (seq line)))))

;; part 1
(reduce + (map calibration-value  (utils/read-input "2023/day1.txt")))

;; part 2
;; using reduce kind of feels like it will get into state machine territory
;; an approach where we take n, then see if the rest of the string starts with
;; "one" "two" "three" etc, seems fine.

(defn pluck-english-from-string [^String s]
  (let [ch (.charAt s 0)]
    (case ch
      (\0  \1 \2 \3 \4 \5 \6 \7 \8 \9) (utils/parse-int (str ch))
      (if-let [m (re-matches #"^(one|two|three|four|five|six|seven|eight|nine).*$" s)]
        (case (second m)
          "one" 1
          "two" 2
          "three" 3
          "four" 4
          "five" 5
          "six" 6
          "seven" 7
          "eight" 8
          "nine" 9)
        nil))))

(defn calibration-value-part2 [s]
  (+
   (*
   (loop [n 0]
    (if (= n (count s))
      (throw (Exception. "did not find number from the left"))
      (if-let [x (pluck-english-from-string (.substring s n))]
        x
        (recur (inc n)))))
   10)
    (loop [n (dec (count s))]
     (if (= n -1)
       (throw (Exception. "did not find number from the right"))
       (if-let [x (pluck-english-from-string (.substring s n))]
         x
         (recur (dec n)))))))

(reduce + (map calibration-value-part2  (utils/read-input "2023/day1.txt")))
