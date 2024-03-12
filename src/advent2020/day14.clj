(ns advent2020.day14
  (:require [utils :as utils]))

(defn parse-mask [mask-line]
  (let [mask-seq (->> mask-line
                      (seq)
                      (map-indexed (fn [n x]
                                     (vector (- 35 n) x)))
                      (remove #(= \X (second %)))
                      (group-by second))]
    (-> mask-seq
        (update-vals #(mapv (fn [n] (bit-shift-left 1 (first n))) %))
        (update-vals #(reduce bit-or %))
        (update-keys #(case % \0 0 \1 1)))))

(defn apply-mask [curr-mask val]
  (-> val
      (bit-or (curr-mask 1))
      (bit-and (bit-not (curr-mask 0)))))

(defn parse-line [line]
  (if-let [m (re-matches #"^mask = ((?:0|1|X){36})$" line)]
    [:set-mask (parse-mask (second m))]
    (if-let [[_ n1 n2] (re-matches #"^mem\[(\d+)\] = (\d+)$" line)]
      [:set-mem (utils/parse-int n1) (utils/parse-int n2)])))

(defn step [[mem mask] [command arg1 arg2]]
  (case command
    :set-mask [mem arg1]
    :set-mem [(assoc mem arg1 (apply-mask mask arg2)) mask]
    ))

(defn answer-part1 [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (map parse-line)
       (reduce step [{} nil])
       (first)
       (vals)
       (reduce +)))

(answer-part1 "day14-example.txt")
(answer-part1 "day14.txt")

  (parse-line "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"))

(apply-mask (parse-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X") 0)

    (-> 11
        (bit-or (bit-shift-left 1 6))
        (bit-and
         (bit-not (bit-shift-left 1 1))))

    (bit-shift-left 1 1)


    (map-indexed vector (seq mask-line))

    (utils/read-input "2020/day14-example.txt")
