(ns advent2020.day16
  (:require [utils :as utils]))

(defn parse-number-list [str-list]
  (->> (.split str-list ",")
       (mapv utils/parse-int)))

(defn parse-field [^String str]
  (let [[_ type & num-strs] (re-matches #"([^:]+): (\d+)\-(\d+) or (\d+)\-(\d+)" str)]
    {type (->> num-strs
       (map utils/parse-int)
       (partition 2)
       (map #(apply vector %)))}))

(defn parse-input [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (utils/split-by "")
       ((fn [[field-info [_ your-ticket-nums] [_ & nearby-tickets]]]
          {:field-info (->> field-info
                            (map parse-field)
                            (into {}))
           :your-ticket (parse-number-list your-ticket-nums)
           :nearby-tickets (mapv parse-number-list nearby-tickets)}))))

(parse-input "day16-example.txt")
(parse-input "day16.txt")

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
        intervals (merge-intervals (apply concat (vals (parsed-input :field-info))))]
    (->> (flatten (parsed-input :nearby-tickets))
       (map #(if (contains-point? intervals %)
               nil
               %))
       (remove nil?)
       (reduce +))))

(answer-part1 "day16-example.txt")
(answer-part1 "day16.txt")

;; find all valid tickets (e.g. filter out the invalid ones)

(defn valid-ticket? [intervals ticket]
  (every? (partial contains-point? intervals) ticket))

(defn answer-part2 [filename]
  (let [parsed-input (parse-input filename)
        intervals (merge-intervals (apply concat (vals (parsed-input :field-info))))
        valid-tickets (filterv (partial valid-ticket? intervals)
                               (parsed-input :nearby-tickets))
        candidates (into {}
                         (for [[field-name [i1 i2]] (parsed-input :field-info)]
                           {field-name
                            (->>
                             (for [i (range (count (first valid-tickets)))]
                               (if (every? (partial contains-point? [i1 i2])
                                           (map #(get % i) valid-tickets))
                                 i
                                 nil))
                             (remove nil?)
                             (set))}))
        mapping (loop [candidates candidates
                       mapping {}]
                  (if (empty? candidates)
                    mapping
                    (let [[x s] (->> candidates
                                     (filter #(= (count (second %)) 1))
                                     (first))]
                      (recur
                       (-> candidates
                           (dissoc x)
                           (update-vals #(disj % (first s))))
                       (assoc mapping x (first s))))))]
    (->> mapping
         (filter #(.startsWith (first %) "departure"))
         (vals)
         (map (partial get (parsed-input :your-ticket)))
         (reduce *))))

(answer-part2 "day16-example.txt")
(answer-part2 "day16-example2.txt")
(answer-part2 "day16.txt")
