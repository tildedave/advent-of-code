(ns advent2020.day16
  (:require [utils :as utils]
            [rbt :refer [rbt]]))

(defn parse-number-list [str-list]
  (->> (.split str-list ",")
       (map utils/parse-int)))

(defn parse-field [^String str]
  (->> str
       (re-matches #"\w+: (\d+)\-(\d+) or (\d+)\-(\d+)")
       (rest)
       (map utils/parse-int)
       (partition 2)
       (map #(apply vector %))))

(defn parse-input [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (utils/split-by "")
       ((fn [[field-info [_ your-ticket-nums] [_ & nearby-tickets]]]
          {:field-info (map parse-field field-info)
           :your-ticket (parse-number-list your-ticket-nums)
           :nearby-tickets (map parse-number-list nearby-tickets)}))))
(parse-input "day16-example.txt")

(defn overlap? [[lo1 hi1] [lo2 hi2]]
  (and (< lo1 hi2)
       (< lo2 hi1)))

(defn interval-< [[lo1 hi1] [lo2 hi2]]
  (< hi1 lo2))

(defn interval-> [[lo1 hi1] [lo2 hi2]]
  (< hi2 lo1))

(defn interval-merge [[lo1 hi1] [lo2 hi2]]
  (if (overlap? [lo1 hi1] [lo2 hi2])
    [(min lo1 lo2) (max hi1 hi2)]
    (throw (Exception. "asked to merge two non-overlapping intervals"))))

;; OK RBT time seems to have worked.

(defn answer-part1 [filename]
  (let [parsed-input (parse-input filename)
        tree (rbt (apply concat (parsed-input :field-info))
                  interval-<
                  interval->
                  interval-merge)]
    (->> (flatten (parsed-input :nearby-tickets))
         (map #(if (not (nil? (get tree [% %])))
                 nil
                 %))
         (remove nil?)
         (reduce +))))

(answer-part1 "day16-example.txt")
(answer-part1 "day16.txt")
