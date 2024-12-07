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

     (->> (map parse-line example-lines)
          (filter (fn [[num num-list]] (can-reach? num-list num)))
          (map first)
          (reduce +))

     (->> (map parse-line (utils/read-input "2024/day7.txt"))
          (filter (fn [[num num-list]] (can-reach? num-list num)))
          (map first)
          (reduce +))
