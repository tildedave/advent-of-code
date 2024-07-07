(ns advent2023.day4
  (:require [utils :as utils]
            [clojure.set :as set]))

(def parse-number-list #(map utils/parse-int (.split #"\s+" %)))

(defn parse-card [line]
  (let [[_ card-str winning-num-list card-num-list]
        (re-matches #"^Card\s+(\d+):\s+((?:(?:\d+)\s+)+)\|\s+((?:(?:\d+)\s*)+)$" line)]
    {:number (utils/parse-int card-str)
     :winning-numbers (set (parse-number-list winning-num-list))
     :card-numbers (set (parse-number-list card-num-list))}))

(parse-card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

(defn card-score [parsed-card]
  (let [winning-numbers (set/intersection (:winning-numbers parsed-card) (:card-numbers parsed-card))]
    (if (empty? winning-numbers)
      0
      (bit-shift-left 1 (dec (count winning-numbers))))))

(card-score (parse-card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))

;; part 1
(->> (utils/read-input "2023/day4.txt")
     (map parse-card)
     (map card-score)
     (reduce +))

;; part 2
(defn answer-part2 [lines]
  (->> (map parse-card lines)
       (reduce
        (fn [card-counts {:keys [number winning-numbers card-numbers]}]
          (let [matching-numbers (set/intersection winning-numbers card-numbers)]
            (->> (range (count matching-numbers))
                 (map (partial + 1 number))
                 (map (fn [m] {m (get card-counts number 1)}))
                 (reduce (partial merge-with +) card-counts))))
        (reduce merge {} (map (fn [n] {n 1}) (range 1 (inc (count lines))))))
       (vals)
       (reduce +)))

(answer-part2
 '("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
   "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
   "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
   "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
   "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
   "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))

(answer-part2 (utils/read-input "2023/day4.txt"))
