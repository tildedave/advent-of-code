(ns advent2020.day16
  (:require [utils :as utils]))

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

(defn interval-compare [i1 i2]
  (cond
    (interval-< i1 i2) -1
    (interval-> i1 i2) 1
    :else 0))

;; this is the non-rbt approach, I'm going to use it

(defn merge-intervals [intervals]
  (loop [intervals intervals
         result '[]]
    (if-let [x (first intervals)]
      (recur
       (rest intervals)
       (if (empty? result)
        [x]
        (let [q (group-by (partial interval-compare x) result)]
          (-> (get q 1 [])
              (into [(reduce interval-merge x (q 0))])
              (into (q -1))))))
      result)))

(defn contains-point? [intervals pt]
  (loop [lo 0
         hi (count intervals)]
    (cond (<= hi lo) false
          :else (let [idx (quot (+ hi lo) 2)
                      [l h] (intervals idx)]
                  (cond (<= l pt h) true
                        (< pt l) (recur lo idx)
                        :else (recur (inc idx) hi))))))

(defn answer-part1 [filename]
  (let [parsed-input (parse-input filename)
        intervals (merge-intervals (apply concat (parsed-input :field-info)))]
    (->> (flatten (parsed-input :nearby-tickets))
       (map #(if (contains-point? intervals %)
               nil
               %))
       (remove nil?)
       (reduce +))))

(answer-part1 "day16-example.txt")
(answer-part1 "day16.txt")
