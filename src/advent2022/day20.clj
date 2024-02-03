(ns advent2022.day20
    (:require [advent2022.utils :as utils]))

;; grove positioning system

(def lines (utils/read-resource-lines "input/day20.txt"))
(def parsed-lines (map utils/parse-int lines))

(defn swap [v m n]
  (-> v
      (assoc n (get v m))
      (assoc m (get v n))))

(swap [0 1 2 3 4] 3 4)
(swap {4 5, 3 2} 3 4)

;; this is very inefficient.  how can it be more efficient?

(defn initial [parsed-lines]
  [(into {} (map-indexed (fn [n x] [x n]) parsed-lines))
   (vec parsed-lines)])


(defn step [[pos-set num-list] n]
  (let [di (if (< n 0) -1 1)
        start-idx (pos-set n)
        l (count num-list)]
    (if (not= pos-set (first (initial num-list)))
      (println "inconsistency"
               pos-set (first (initial num-list))))
    (loop
     [i 0
      pos-set pos-set
      num-list num-list]
      (if (= i n)
        [pos-set num-list]
         ;; otherwise swap everything, recur on i + direction
        (let [curr-idx (mod (+ start-idx i) l)
              next-idx (mod (+ curr-idx di) l)]
          (recur
           (+ i di)
         ;; we want to swap position i and i + di
           (swap pos-set (get num-list curr-idx) (get num-list next-idx))
           (swap num-list curr-idx next-idx)))))))


(def foo (step (initial parsed-lines) (first parsed-lines)))

(def bar (initial parsed-lines))

((first bar) 6362)

(first foo)
(second foo)
(= (first foo) (initial (second foo)))
((first foo) (ini))
(nth (second foo) 1704)

parsed-lines
((first (initial parsed-lines)) 6362)

(-> (initial parsed-lines)
    (step (nth parsed-lines 0))
    (step (nth parsed-lines 1)))

;; (reduce step (initial parsed-lines) parsed-lines)

(defn answer [parsed-lines]
  (let [[pos-set num-list] (reduce step (initial parsed-lines) parsed-lines)]
    (loop [i 0
           idx (pos-set 0)
           num 0]
      (let [next-idx (mod (inc idx) (count num-list))]
      (cond
        (= i 3000) (+ num (num-list idx))
        (= i 2000)
          (recur (inc i) next-idx (+ num (num-list idx)))
        (= i 1000)
          (recur (inc i) next-idx (+ num (num-list idx)))
        :else (recur (inc i) next-idx num))))))

;; part 1 answer
;; (println (answer parsed-lines))
