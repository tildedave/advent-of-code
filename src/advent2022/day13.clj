(ns advent2022.day13
  (:require [advent2022.utils :as utils]
            [clojure.string :as string]))

(def lines (utils/read-resource-lines "input/day13.txt"))


(defn integer-or-list [n]
  (cond
    (integer? n) :integer
    (vector? n) :list
    (empty? n) :list
    :else :error))

(defn packet-compare
  ;; -1 if p1 less, 0 if equal, 1 if p2 greater.
  [p1 p2]
  (case [(integer-or-list p1) (integer-or-list p2)]
    [:integer :integer] (compare p1 p2)
    [:list :list]
    (cond
      (and (empty? p1) (empty? p2)) 0
      (empty? p1) -1
      (empty? p2) 1
      :else
      (let [x1 (first p1) x2 (first p2)
            c (packet-compare x1 x2)]
        (if (not= c 0)
          c
          (packet-compare (subvec p1 1) (subvec p2 1)))))
    [:list :integer] (packet-compare p1 [p2])
    [:integer :list] (packet-compare [p1] p2)))

(defn correct-order? [p1 p2] (= (packet-compare p1 p2) -1))

;; part 1
(->> lines
     (partition-by (fn [str] (= str "")))
     (filter (fn [seq] (not= seq (list ""))))
     (map (fn [l] (map #(string/replace % "," " ") l)))
     ;; string to form 😎
     (map (fn [l] (map #(eval (read-string %)) l)))
     (map (fn [l] (correct-order? (first l) (second l))))
     (map-indexed (fn [n v] [n v]))
     (filter (fn [[_ b]] b))
     (map (fn [[n _]] (inc n)))
     (reduce +))



(packet-compare [1 1 3 1 1] [1 1 5 1 1])
