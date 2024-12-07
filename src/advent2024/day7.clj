(ns advent2024.day7
  (:require [utils :as utils]))

(last [1 2 3 4 4])

(defn can-reach? [num-list num]
  (let [l (count num-list)
        final (nth num-list (dec l))]
    (cond
      (= l 1) (= (first num-list) num)
      (zero? (mod num final)) (recur (subvec num-list 0 (dec l)) (quot num final))
      :else (recur (subvec num-list 0 (dec l)) (- num final)))))

(defn ends-with-str? [num suffix]
  (.endsWith (str num) (str suffix)))

(defn strip-num-suffix [num suffix]
  (let [result (parse-long (.substring (str num) 0 (.lastIndexOf (str num) (str suffix))))]
    (if (nil? result) 0 result)))

(strip-num-suffix 4555 5)

(defn can-reach-brute-force? [num-list num]
  (let [l (count num-list)
        final (nth num-list (dec l))]
    (cond
      (= l 1) (= (first num-list) num)
      (zero? (mod num final))
      (or
       (can-reach-brute-force? (subvec num-list 0 (dec l)) (quot num final))
       (can-reach-brute-force? (subvec num-list 0 (dec l)) (- num final)))
      :else
      (recur (subvec num-list 0 (dec l)) (- num final)))))

(defn can-reach-brute-force-part2? [num-list num]
  (let [l (count num-list)
        final (nth num-list (dec l))]
    (cond
      (< num 0) false
      (= l 1) (= (first num-list) num)
      :else
      (or (when (zero? (mod num final))
            (can-reach-brute-force-part2? (subvec num-list 0 (dec l)) (quot num final)))
          (when (ends-with-str? num final)
            (can-reach-brute-force-part2? (subvec num-list 0 (dec l)) (strip-num-suffix num final)))
          (recur (subvec num-list 0 (dec l)) (- num final))))))

(defn parse-line [s]
  (->> (re-matches #"(\d+):((?: \d+)+)" s)
       (rest)
       ((fn [[a r]] [(parse-long a) (vec (utils/parse-number-list (.trim r)))]))))

(parse-line "190: 10 19")

(def example-lines '("190: 10 19"
                     "3267: 81 40 27"
                     "83: 17 5"
                     "156: 15 6"
                     "7290: 6 8 6 15"
                     "161011: 16 10 13"
                     "192: 17 8 14"
                     "21037: 9 7 18 13"
                     "292: 11 6 16 20"))

(can-reach-brute-force-part2? [15 6] 156)
(can-reach-brute-force-part2? [6 8 6 15] 7290)

(/ 7290 15)
(->> (map parse-line example-lines)
     (filter (fn [[num num-list]] (can-reach-brute-force? num-list num)))
     (map first)
     (reduce +))

(->> (map parse-line example-lines)
     (filter (fn [[num num-list]] (can-reach-brute-force-part2? num-list num)))
     (map first)
     (reduce +))

(->> (map parse-line (utils/read-input "2024/day7.txt"))
     (filter (fn [[num num-list]] (can-reach-brute-force? num-list num)))
     (map first)
     (reduce +))

(->> (map parse-line (utils/read-input "2024/day7.txt"))
     (filter (fn [[num num-list]] (can-reach-brute-force-part2? num-list num)))
     (map first)
     (reduce +)
     (time))
;; 19ms
