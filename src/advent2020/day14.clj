(ns advent2020.day14
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]))

(def ^:dynamic part2? false)

(defn parse-mask [mask-line]
  (let [mask-seq (->> mask-line
                      (seq)
                      (map-indexed (fn [n x]
                                     (vector (- 35 n) x)))
                      (group-by second))
        x-seq (mapv #(bit-shift-left 1 (first %)) (mask-seq \X))]
    (-> mask-seq
        (update-vals #(mapv (fn [n] (bit-shift-left 1 (first n))) %))
        (update-vals #(reduce bit-or %))
        (update-keys #(case % \0 0 \1 1 \X \X))
        (assoc :floating-masks x-seq))))

(defn apply-mask [curr-mask val]
  (if part2?
    (let [base (-> val
                   (bit-or (get curr-mask 1 0)))]
      ;; for each combo of floating masks,
      ;; do the apply.
      ;; this will be a for loop that both includes/
      ;; does not include them.
      (->> (curr-mask :floating-masks)
           (map #(vector {0 [%]} {1 [%]}))
           (apply combo/cartesian-product)
           (map #(apply merge-with concat %))
           (map #(update-vals % (fn [s] (reduce bit-or s))))
           (map (fn [mask]
                (-> base
                    (bit-or (get mask 1 0))
                    (bit-and (bit-not (get mask 0 0))))))
      ))
    (-> val
        (bit-or (get curr-mask 1 0))
        (bit-and (bit-not (get curr-mask 0 0))))))

(binding [part2? true] (apply-mask {0 68719476684, \X 33, 1 18, :floating-masks [32 1]} 42))

(defn mask-to-bits [m]
  (loop [i 0 m m res '()]
    (if (= i 36) res
        (recur (inc i)
               (bit-shift-right m 1)
               (cons (if (bit-test m 0) 1 0) res)))))

;; then we apply EVERY COMBINATION OF floating bit.

(defn parse-line [line]
  (if-let [m (re-matches #"^mask = ((?:0|1|X){36})$" line)]
    [:set-mask (parse-mask (second m))]
    (if-let [[_ n1 n2] (re-matches #"^mem\[(\d+)\] = (\d+)$" line)]
      [:set-mem (utils/parse-int n1) (utils/parse-int n2)])))

(defn step [[mem mask] [command arg1 arg2]]
  (case command
    :set-mask [mem arg1]
    :set-mem [(assoc mem arg1 (apply-mask mask arg2)) mask]))

(defn answer-part1 [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (map parse-line)
       (reduce step [{} nil])
       (first)
       (vals)
       (reduce +)))

(answer-part1 "day14-example.txt")
(answer-part1 "day14.txt")

(defn step2 [[mem mask] [command arg1 arg2]]
  (case command
    :set-mask [mem arg1]
    :set-mem [(reduce
                (fn [mem addr] (assoc mem addr arg2))
                mem
                (apply-mask mask arg1))
              mask]))

(defn answer-part2 [filename]
  (binding [part2? true]
    (->> (utils/read-input (format "2020/%s" filename))
         (map parse-line)
         (reduce step2 [{} nil])
         (first)
         (vals)
         (reduce +))))

(answer-part2 "day14-example2.txt")
(answer-part2 "day14.txt")
