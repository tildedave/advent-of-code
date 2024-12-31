(ns advent2017.day1
  (:require [utils :as utils]))

(defn answer-part1 []
  (let [num-seq (->> (utils/read-input "2017/day1.txt")
                     (first)
                     (seq)
                     (map #(- (int %) (int \0))))]
    (reduce +
            (map
             (fn [n1 n2]
               (if (= n1 n2) n1 0))
             num-seq (concat (rest num-seq) (list (first num-seq)))))))

(defn part2 [num-seq]
  (let [q (quot (count num-seq) 2)]
    (reduce +
            (map
             (fn [n1 n2]
               (if (= n1 n2) n1 0))
             num-seq (concat (drop q num-seq) (take q num-seq))))))

(defn parse-numbers [nums]
  (->> nums
       (seq)
       (map #(- (int %) (int \0)))))

(part2 (parse-numbers "12131415"))

(defn answer-part2 []
   (part2 (parse-numbers (first (utils/read-input "2017/day1.txt")))))

(answer-part2)
