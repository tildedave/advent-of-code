(ns advent2017.day6
  (:require [utils :as utils]))

;; this is a seed sowing game

(defn make-map [nums]
  (reduce merge (map-indexed hash-map nums)))

(->> (make-map [0 7 7 7])
     (sort-by second >)
     (partition-by second)
     (first)
     (sort-by first)
     (first)
     (first))

(partition-by second (sort-by second > (make-map [0 2 7 0])))

(defn max-idx [num-map]
  (->> num-map
       (sort-by second >)
       (partition-by second)
       ;; the first element in the list is those with the max values, now we
       ;; need to find the lowest index.
       (first)
       (sort-by first)
       (first)
       (first)))

(defn step [num-map]
  (let [idx (max-idx num-map)
        inc-idx #(mod (inc %) (count num-map))]
    (first
     (reduce
     (fn [[num-map idx] _]
       [(update num-map idx inc)
        (inc-idx idx)])
     [(assoc num-map idx 0) (inc-idx idx)]
     (range (num-map idx))))))

(step (make-map [0 2 7 0]))

(defn answer [nums]
  (reduce
   (fn [acc num-map]
     (if (contains? acc num-map)
       (reduced (count acc))
       (conj acc num-map)))
   #{}
   (iterate step (make-map nums))))

(answer [0 2 7 0])
(answer (map utils/parse-int (.split #"\s+" (first (utils/read-input "2017/day6.txt")))))

(defn answer-part2 [nums]
  (reduce
   (fn [[acc n] num-map]
     (if (contains? acc num-map)
       (reduced (- n (acc num-map)))
       [(assoc acc num-map n) (inc n)]))
   [{} 0]
   (iterate step (make-map nums))))

(answer-part2 [0 2 7 0])
(answer-part2 (map utils/parse-int (.split #"\s+" (first (utils/read-input "2017/day6.txt")))))
