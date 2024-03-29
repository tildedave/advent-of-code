(ns advent2020.day2
  (:require [utils :as utils]))

(set! *warn-on-reflection* true)
(def ^:dynamic part2? false)

(defn parse-policy [policy]
  (let [[num-range letter] (.split ^String policy " ")
        [lo high] (->> (.split ^String num-range "-")
                       (map utils/parse-int))]
    {:low lo :high high :letter (.charAt ^String letter 0)}))

(defn parse-line [line]
  (let [[start password] (.split ^String line ": ")]
    {:policy (parse-policy start)
     :password password}))

(seq "abcde")

(defn is-password-valid? [{:keys [letter low high]} password]
  (case part2?
    false (<= low
              (->> password (seq) (filter (partial = letter)) (count))
              high)
    true
    (utils/xor
     (= ^Character (.charAt ^String password (dec low)) letter)
     (= ^Character (.charAt ^String password (dec high)) letter))))

(defn is-valid? [{:keys [password policy]}]
  (is-password-valid? policy password))

(let [{:keys [password policy]} (parse-line "1-3 a: abcde")]
  (is-password-valid? policy password))

(defn answer-part1 [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (map parse-line)
       (map is-valid?)
       (remove false?)
       (count)))

(answer-part1 "day2-example.txt")
(answer-part1 "day2.txt")

(defn answer-part2 [filename]
  (binding [part2? true]
  (->> (utils/read-input (format "2020/%s" filename))
       (map parse-line)
       (map is-valid?)
       (remove false?)
       (count))))

(answer-part2 "day2-example.txt")
(answer-part2 "day2.txt")
