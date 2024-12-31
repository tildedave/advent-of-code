(ns advent2015.day16
  (:require [utils :as utils]
            [clojure.string :as string]))

(def ^:dynamic part2? false)

(defn parse-facts [line]
  (->> (.split line ", ")
       (map #(apply hash-map (.split % ": ")))
       (into {})
       (#(update-vals % utils/try-parse-int))))
    ;;   (apply concat)
    ;;   (apply hash-map)
    ;;   (update-vals utils/try-parse-int)))

(every? true? '(false true true))

(defn facts-match? [partial-facts full-facts]
  (if part2?
    (every? true? (for [[fact num] partial-facts]
                    ((case fact
                      ("cats" "trees") <
                      ("pomeranians" "goldfish") >
                      =) (full-facts fact) num)))
    (every? true? (for [[fact num] partial-facts]
                    (= (full-facts fact) num)))))

(defn answer-part1 []
  (let [full-facts (->> (utils/read-input "2015/day16-correct.txt")
                        (string/join ", ")
                        (parse-facts))]
    (->> (utils/read-input "2015/day16.txt")
         (filter #(let [[sue-name partial-facts] (.split % ": " 2)]
                    (facts-match? (parse-facts partial-facts)
                                  full-facts)))
         (first)
         (#(.split % ": "))
         (first)
         (#(.split % " "))
         (second)
         (utils/parse-int)
         )))

(answer-part1)

(defn answer-part2 []
  (binding [part2? true] (answer-part1)))

(answer-part2)
