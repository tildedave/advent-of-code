(ns advent2022.day13
    (:require [advent2022.utils :as utils]
              [clojure.string :as string]))

  (def lines (utils/read-resource-lines "input/day13-example.txt"))


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

  (def parsed-packets
    (->> lines
         (partition-by (fn [str] (= str "")))
         (filter (fn [seq] (not= seq (list ""))))
         (map (fn [l] (map #(string/replace % "," " ") l)))
     ;; string to form 😎
         (map (fn [l] (map #(eval (read-string %)) l)))))

;; part 1
  (->> parsed-packets
       (map (fn [l] (correct-order? (first l) (second l))))
       (map-indexed vector)
       (filter (fn [[_ b]] b))
       (map (fn [[n _]] (inc n)))
       (reduce +))

;; part 2
  (->>
   (-> parsed-packets
       (conj [[[6]]])
       (conj [[[2]]]))
   (apply concat)
   (sort-by identity packet-compare)
   (map-indexed vector)
   (filter (fn [[_ b]] (or (= b [[2]]) (= b [[6]]))))
   (map first)
   (map inc)
   (reduce *))
