(ns advent2017.day17
  (:require [utils :as utils]))

(defn initial-state []
  (let [a (atom {:next nil :num 0})]
    (swap! a #(assoc % :next a))
    {:node a :next-number 1}))

;; (:num (deref (:next (deref (initial-state)))))

(defn print-node [node n]
  (first
   (reduce
   (fn [[acc a] _]
     (let [{:keys [next num]} (deref a)]
       [(conj acc num) next]))
   [[] node]
   (range n))))

(print-node (:node (initial-state)) 3)

(defn step [steps-per-insert {:keys [node next-number]}]
  (let [node (reduce
              (fn [node _]
                (:next (deref node)))
              node
              (range steps-per-insert))
        new-node (atom
                  {:next (:next (deref node)) :num next-number})]
    (swap! node #(assoc % :next new-node))
    {:node new-node :next-number (inc next-number)}))

(defn answer [steps-per-insert]
  (->>
   (print-node
    (:node (nth (iterate (partial step steps-per-insert) (initial-state)) 2017)) 2)
   (second)
  ))

(answer 3)
(answer (->> (utils/read-input "2017/day17.txt")
             (first)
             (utils/parse-int)))

(defn number-after-0 [steps-per-insert]
  (first
   (reduce
   (fn [[last-zero idx] n]
     (let [next-idx (mod (+ idx steps-per-insert) (dec n))]
       [(if (zero? next-idx)
          (dec n)
          last-zero)
        (mod (inc next-idx) n)]))
   [0 0]
   (range 2 50000000))))

(number-after-0 (->> (utils/read-input "2017/day17.txt")
                                    (first)
                                    (utils/parse-int)))
