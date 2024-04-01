(ns advent2020.day23
  (:require [utils :as utils]
            [clojure.string :as string]))

(defn parse-input [filename]
  (->> filename
       (format "2020/%s")
       (utils/read-input)
       (first)
       (seq)
       (map #(utils/parse-int (str %)))
       (vec)
       (#(hash-map :min-cup (reduce min %)
                   :max-cup (reduce max %)
                   :cups %))))

(parse-input "day23-example.txt")

;; presumably part 2 will require us to be smarter about this
;; (somehow), but we'll just do part 1 naively.
;; we'll always assume the first one is at 0 since that makes my job a little
;; easier.

(subvec [1 2 3] 3 3)

(defn step [state]
  (let [{:keys [min-cup max-cup cups]} state
        first-cup  (get cups 0)
        picked-up-cups (subvec cups 1 4)
        cup-set (set picked-up-cups)
        next-label (loop [n (dec first-cup)]
                     (cond
                       (contains? cup-set n) (recur (dec n))
                       (< n min-cup) (recur max-cup)
                       :else n))
        next-idx (.indexOf cups next-label)
        new-ordering (-> [next-label]
                         (into picked-up-cups)
                         (into (subvec cups (inc next-idx)))
                         (into [first-cup])
                         (into (subvec cups 4 next-idx)))
        current-cup-idx (.indexOf new-ordering first-cup)
        next-cup-idx (mod (inc current-cup-idx) (count cups))]
    (assoc state :cups
           (-> (subvec new-ordering next-cup-idx)
               (into (subvec new-ordering 0 next-cup-idx))))))

(defn answer [{:keys [min-cup max-cup cups]}]
  (let [idx (.indexOf cups 1)
        inc-idx (mod (inc idx) (count cups))]
    (-> (subvec cups inc-idx)
        (into (subvec cups 0 idx))
        (#(string/join "" %)
        ))))

(defn answer-part1 [filename]
  (-> (iterate step (parse-input filename))
       (nth 101)
       (answer)
       ))

(answer-part1 "day23-example.txt")

(answer
(last (take 101 (iterate step (parse-input "day23-example.txt")))))
