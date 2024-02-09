(ns advent2022.day20
    (:require [advent2022.utils :as utils]))

;; grove positioning system

(def example-lines (utils/read-resource-lines "input/day20-example.txt"))
(def input-lines (utils/read-resource-lines "input/day20.txt"))

(defn mix-idx [num-list idx]
  ;; idx is the place in the original list.
  (let [[loc x] (->> num-list
                  ;; yes, this double indexes it.
                  (map-indexed vector)
                  (filter (fn [[_ [orig-idx]]] (= orig-idx idx)))
                  (first))
        ;; OK, it is probably just simplest reset the list so that
        ;; loc is at the front.  then the cuts are much clearer.
        [_ n] x
        num-list (into (subvec num-list loc) (subvec num-list 0 loc))
        new-idx (mod n (dec (count num-list)))]
    (cond (= new-idx 0) num-list
        :else (into (conj (subvec num-list 1 (inc new-idx)) x)
                    (subvec num-list (inc new-idx))))))

(subvec [1 2 3 4] 1 2)

(defn mix-list [lines]
  (let [num-list (->> lines
                      (map utils/parse-int)
                      (map-indexed vector)
                      (vec))
        mixed-list (reduce mix-idx num-list (range 0 (count num-list)))]
    (mapv second mixed-list)))

(defn answer [num-list]
  (let [num-list (vec num-list)]
  (loop [i 0
         idx (.indexOf num-list 0)
         result (list)]
    (let [next-idx (mod (inc idx) (count num-list))]
      (cond
        (= i 3000) (cons (num-list idx) result)
        (= i 2000)
        (recur (inc i) next-idx (cons (num-list idx) result))
        (= i 1000)
        (recur (inc i) next-idx (cons (num-list idx) result))
        :else (recur (inc i) next-idx result))))))


(reduce + (answer (mix-list example-lines)))
(reduce + (answer (mix-list input-lines)))

;; part 1 answer
;; (println (answer parsed-lines))
