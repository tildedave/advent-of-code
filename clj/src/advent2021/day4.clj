(ns advent2021.day4
  (:require [utils :as utils]
            [clojure.string :as string]))

(defn lines-to-bingo [lines]
  (let [rows (->> lines
       (map string/trim)
       (map #(string/split % #"\s+"))
       (map #(map utils/parse-int %)))
        columns (for [n (range (count (first rows)))]
                  (map #(nth % n) rows))]
    (concat
     (map set rows)
     (map set columns))))

;; for now a bingo card is just a list of sets.
(defn mark-number [bingo-card num]
  (map #(disj % num) bingo-card))

(defn is-complete? [bingo-card]
  (if (some empty? bingo-card) true false))

(defn score [bingo-card num]
  ;; take all remaining numbers
  (* num
     (->> bingo-card
       (reduce concat)
       (set)
       (reduce +))))

(defn answer-part1 [lines]
  (let [parsed-lines (->> lines
                          (partition-by #(= % ""))
                          (remove #(= % (list ""))))
        numbers (map utils/parse-int (.split (first (first parsed-lines)) ","))
        bingo-cards (map lines-to-bingo (rest parsed-lines))]
    (loop [bingo-cards bingo-cards
           numbers numbers]
      (let [num (first numbers)
            bingo-cards (map #(mark-number % num) bingo-cards)
            complete-cards (filter is-complete? bingo-cards)]
        (if (seq complete-cards)
          (score (first complete-cards) num)
          (recur bingo-cards (rest numbers) ))))))

(answer-part1 (utils/read-resource-lines "input/day4-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day4.txt"))


;; part 2 is really the same, except we wait for the final board to win.

(defn answer-part2 [lines]
  (let [parsed-lines (->> lines
                          (partition-by #(= % ""))
                          (remove #(= % (list ""))))
        numbers (map utils/parse-int (.split (first (first parsed-lines)) ","))
        bingo-cards (map lines-to-bingo (rest parsed-lines))]
    (loop [bingo-cards bingo-cards
           numbers numbers]
      (let [num (first numbers)
            bingo-cards (map #(mark-number % num) bingo-cards)
            complete-cards (filter is-complete? bingo-cards)
            remaining-cards (remove is-complete? bingo-cards)]
        (if (empty? remaining-cards)
          (score (first complete-cards) num)
          (recur remaining-cards (rest numbers)))))))

(answer-part2 (utils/read-resource-lines "input/day4-example.txt"))
(answer-part2 (utils/read-resource-lines "input/day4.txt"))
