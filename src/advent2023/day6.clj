(ns advent2023.day6
  (:require [utils :as utils]
            [clojure.math :as math]))

(defn boat-distance [speed num-seconds]
  (* speed num-seconds))

(defn ways-to-beat-record [num-seconds distance]
  (->>
   (range num-seconds)
   (map (fn [n] (boat-distance n (- num-seconds n))))
   (drop-while #(<= % distance))
   (take-while #(> % distance))
   (count)
  ))

(ways-to-beat-record 30 200)

(defn parse-distance-and-times [lines]
  (->> lines
       (map #(-> (.split #":\s+" % 2)
                 (second)
                 (utils/parse-number-list)))
       (apply map vector)))

(parse-distance-and-times
 '("Time:      7  15   30"
   "Distance:  9  40  200"))

(defn answer-part1 [lines]
  (->> lines
       (parse-distance-and-times)
       (map (partial apply ways-to-beat-record))
       (reduce *)))

(answer-part1
   '("Time:      7  15   30"
     "Distance:  9  40  200"))

(answer-part1 (utils/read-input "2023/day6.txt"))

(defn parse-distance-and-times-part2 [lines]
  (->> lines
       (mapv #(-> (.split #":\s+" % 2)
                 (second)
                 (.replaceAll "\\s+" "")
                 (parse-long)))))

(parse-distance-and-times-part2 '("Time:      7  15   30"
                                  "Distance:  9  40  200"))

;; just pure brute force
;; I made cutting off a bit smarter
(apply ways-to-beat-record (parse-distance-and-times-part2 (utils/read-input "2023/day6.txt")))

;; it seems like there's an alternative solution here, where we can solve
;; the equation as a quadratic ax^2 + bx + c - distance, solve for the roots,
;; then see the distance between the roots?
;; c = 0 because x = 0 -> -distance
;; a + b - 9 = -3
;; a + b = 6
;; 4a + 2b - 9 = 1 ==> 4(6 - b) + 2b = 10 ==> 24 - 2b = 10 ==> b = 7
;; 9a + 3b - 9 = 3

(let [b 7
      a -1]
  (+ (* 9 a) (* 3 b)))

;; yes, that approach works.
;; the only "difficulty" is that we need to get integers in range.

(defn solve-quadratic [time distance]
  (let [a-plus-b (boat-distance 1 (- time 1)) ;; a + b = whatever
        _4a-plus-2b (boat-distance 2 (- time 2)) ;; 4a + 2b = whatever
        ;; so a = x - b
        ;; 4(x - b) + 2b = y
        ;; -2b = y - 4 * x
        ;; b =  (y - 4 * x) / -2
        b (/ (- _4a-plus-2b (* 4 a-plus-b)) -2)
        a (- a-plus-b b)
        discrim (- (* b b) (* 4 a (- distance)))]
    ;; a is negative so lower bound on left, upper bound on right
    [(/ (+ (- b) (math/sqrt discrim)) (* 2 a))
     (/ (- (- b) (math/sqrt discrim)) (* 2 a))]))



(defn ways-to-beat-record-smarter [time distance]
  (let [[lower upper] (solve-quadratic time distance)
        lower (if (= (math/ceil lower) lower) (+ lower 1) lower)
        upper (if (= (math/ceil upper) upper) (- upper 1) upper)]
    (int (inc (- (math/floor upper) (math/ceil lower))))))

(assert (=
         (ways-to-beat-record-smarter 15 40)
         (ways-to-beat-record 15 40)))
(assert (= (ways-to-beat-record-smarter 30 200)
           (ways-to-beat-record 30 200)))


(defn answer-part1-smarter [lines]
  (->> lines
       (parse-distance-and-times)
       (map (partial apply ways-to-beat-record-smarter))
       (reduce *)))

(answer-part1-smarter (utils/read-input "2023/day6.txt"))
;; instant
(apply ways-to-beat-record-smarter (parse-distance-and-times-part2 (utils/read-input "2023/day6.txt")))
