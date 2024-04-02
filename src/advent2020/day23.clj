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
                   :current (first %)
                   :end (last %)
                   :next
                   (transient (into {} (map vector % (into (subvec % 1) [(get % 0)]))))))))

(parse-input "day23-example.txt")

;; (def next {7 3, 1 2, 4 6, 6 7, 3 8, 2 5, 9 1, 5 4, 8 9})
;; (nth (iterate next 3) 4)

(defn step [state]
  (let [{:keys [min-cup max-cup current next]} state
        next-seq (iterate next current)
         ;; pick up the next 3 cups
        next (assoc! next current (nth next-seq 4))
        picked-up-cups (->> next-seq (rest) (take 3))
        cup-set (set picked-up-cups)
        destination-cup (loop [n (dec current)]
                          (cond
                            (contains? cup-set n) (recur (dec n))
                            (< n min-cup) (recur max-cup)
                            :else n))
         ;; destination points to the first in the picked-up sequence
         ;; last in the picked up sequence points to the old destination
         ;; pointer.
        destination-next (next destination-cup)
        next (assoc! next destination-cup (first picked-up-cups))
        next (assoc! next (last picked-up-cups) destination-next)]
    (assoc state :next next :current (next current))))

(defn answer [{:keys [next]}]
  (->> (iterate next 1)
       (rest)
       (partition-by (partial = 1))
       (first)
       (#(string/join "" %))))

(defn answer-part1 [filename]
  (-> (iterate step (parse-input filename))
      (nth 100)
      (answer)))

(answer-part1 "day23-example.txt")
(answer-part1 "day23.txt")

(conj '(2 3 4 5) 1)
;; so we need to
(defn parse-input-p2 [filename]
  (let [parsed-input (parse-input filename)
        {:keys [max-cup next end current]} parsed-input
        rest-numbers (vec (range (inc max-cup) 1000001))]
    (assoc parsed-input
           :max-cup 1000000
           :next (reduce (fn [state [k v]]
                           (assoc! state k v))

                        (assoc! next end (inc max-cup))
                        (map vector rest-numbers (into (subvec rest-numbers 1) [current]))))))

(defn answer-part2 [filename]
  (let [{:keys [next]} (nth (iterate step (parse-input-p2 filename)) 10000000)]
    (* (next 1) (next (next 1)))))

(time (answer-part2 "day23-example.txt"))
(time (answer-part2 "day23.txt"))
