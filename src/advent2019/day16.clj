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
  (mapv #(utils/parse-int (str %)) (seq s)))

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

(as-> (parse-input "80871224585914546619083218645595") a
      (iterate apply-phase a))

;; so part 2 needs to be sneakier
;; for part 2, as long as the offset is > halfway point,
;; the pattern is all 1s, so we can compute the digits on
;; the answer directly.
;; I suppose dynamic programming is the simplest
;; I wish there were a more elegant way to do this in Clojure.
;; I could of course just re-implement my golang logic, but
;; that seems against the spirit of Clojure :-)

;; the way I did it before: compute the digits downwards.
;; I guess I also didn't compute the final digit since it's always the
;; same, but whatever.

(defn initial-digits [input-signal]
  (repeat 100 (last input-signal)))


(count (parse-input "80871224585914546619083218645595"))
(mod (dec (* 32 10000)) 32)

(defn digits [input-signal prev-digits idx]
  (loop [last-digit (nth input-signal (mod idx (count input-signal)))
         prev-digits prev-digits
         result []]
    (if-let [x (first prev-digits)]
      (let [next-digit (mod (+ last-digit x) 10)]
        (recur
         next-digit
         (rest prev-digits)
         (conj result next-digit)))
      result)))

(utils/parse-int (.substring "03036732577212944063491565474664" 0 7))

(defn answer-part2 [line]
  (let [is (parse-input line)
        offset (utils/parse-int (.substring line 0 7))]
    (->>
     [(initial-digits is) (dec (* (count is) 10000))]
     (iterate
      (fn [[prev-digits idx]]
        [(digits is prev-digits (dec idx)) (dec idx)]))
     ;; I'm not certain why we need to tweak everything by 1,
     ;; I'd guess my indices are off by one.
     (drop-while (fn [[_ idx]] (not (<= offset idx (+ offset 7)))))
     (take 8)
     (map first)
     (map last)
     (reverse)
     (string/join)
     )))

(answer-part2 "03036732577212944063491565474664")
(answer-part2 "02935109699940807407585447034323")
(answer-part2 "03081770884921959731165446850517")
(time (answer-part2 (utils/read-input-line "2019/day16.txt")))
