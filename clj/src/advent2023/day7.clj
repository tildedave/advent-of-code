(ns advent2023.day7
  (:require [clojure.core.match :refer [match]]
            [utils :as utils]))

(def ^:dynamic part2? false)

(def hand-strength {:five-of-a-kind 6
                    :four-of-a-kind 5
                    :full-house 4
                    :three-of-a-kind 3
                    :two-pair 2
                    :one-pair 1
                    :high-card 0})

(def hand-type
  (fn [hand]
    (let [frequencies (as-> (seq hand) s
                        (group-by identity s)
                        (update-vals s count))
          frequencies (if part2?
                        (let [num-jokers (get frequencies \J 0)
                              next-most-popular (->> (dissoc frequencies \J)
                                                     (sort-by second >)
                                                     (first)
                                                     (first))]
                          (if (nil? next-most-popular)
                             ;; there is an all-joker hand in the input.
                            {\J 5}
                            (-> frequencies
                                (dissoc \J)
                                (update next-most-popular (partial + num-jokers)))))
                        frequencies)]
      (match
       (->> frequencies
            (map second)
            (sort >)
            (take 2)
            (vec))
        [5 & _] :five-of-a-kind
        [4 & _] :four-of-a-kind
        [3 2] :full-house
        [3 & _] :three-of-a-kind
        [2 2] :two-pair
        [2 & _] :one-pair
        _ :high-card))))

(hand-type "JJJJJ")

(defn card-strength [ch]
  (let [m {\A 14 \K 13 \Q 12 \J 11, \T 10 \9 9 \8 8 \7 7 \6 6 \5 5 \4 4 \3 3 \2 2}]
    (if part2?
      (get (dissoc m \J) ch)
      (get m ch))))

(defn hand-compare [hand1 hand2]
  (case
   (compare
    (hand-strength (hand-type hand1))
    (hand-strength (hand-type hand2)))
    -1 -1
    1 1
    0 (loop [hand1 (seq hand1)
             hand2 (seq hand2)]
        (if (empty? hand1)
          0
          (let [[x1 & x1-rest] hand1
                [x2 & x2-rest] hand2]
            (if (= x1 x2)
              (recur x1-rest x2-rest)
              (compare (card-strength x1) (card-strength x2))))))))

(defn parse-hands-and-bids [^String line]
  (let [[hand-str bid-str] (.split line " " 2)]
    {:hand hand-str :bid (utils/parse-int bid-str)}))

(def example
  '("32T3K 765"
  "T55J5 684"
  "KK677 28"
  "KTJJT 220"
  "QQQJA 483"))

(hand-compare "32T3K" "QQQJA")

(assert (= 1 (hand-compare "33332" "2AAAA")))
(assert (= 1 (hand-compare "77888" "77788")))

(defn answer-part1 [lines]
  (->> lines
       (map parse-hands-and-bids)
       (sort-by :hand hand-compare)
       (map-indexed (fn [n {:keys [bid]}] (* (inc n) bid)))
       (reduce +)))

;; correct
(answer-part1 example)
(answer-part1 (utils/read-input "2023/day7.txt"))

(assert (binding [part2? true]
          (= :four-of-a-kind (hand-type "QJJQ2"))))

;; correct
(binding [part2? true] (answer-part1 (utils/read-input "2023/day7.txt")))
(binding [part2? true] (answer-part1 example))
