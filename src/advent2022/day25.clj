(ns advent2022.day25
  (:require [utils :as utils]
            [clojure.string :as string]))

;; leading zero logic
(defn num-to-snafu [num]
  (let [digits (loop [num num
         result '()]
    ;; (println "num is" num "result is" result)
    (if (zero? num) (conj result "0")
        (let [[str carryover] (case (mod num 5)
                                0 ["0" 0]
                                1 ["1" 0]
                                2 ["2" 0]
                                3 ["=" 1];; carryover
                                4 ["-" 1])]
          (recur (+ (quot num 5) carryover)
                 (conj result str)))))
        digits (if (= (first digits) "0") (rest digits) digits)]
    (cond
      (empty? digits) "0"
      :else (string/join digits))))

;; obviously this would be bad for real math
(defn pow [x n]
  (reduce * (repeat n x)))

(defn snafu-to-num [str]
  (loop [digit-list (reverse (seq str))
         i 0
         num 0]
    (if (empty? digit-list) num
        (let [[factor place] (case (first digit-list)
                               \0 [0 0]
                               \1 [1 0]
                               \2 [2 0]
                               \= [3 -1]
                               \- [4 -1])]
          (recur
           (rest digit-list)
           (inc i)
           (+ num (* place (pow 5 (inc i))) (* factor (pow 5 i))))))))

(println (snafu-to-num "1121-1110-1=0"))


(num-to-snafu 4890)

(for [n (range 0 20)]
  (println n (num-to-snafu n)))

(def example-lines (utils/read-resource-lines "input/day25-example.txt"))
(def input-lines (utils/read-resource-lines "input/day25.txt"))
(defn answer [lines]
  (->> lines
       (map snafu-to-num)
       (reduce +)
       (num-to-snafu)))

(answer example-lines)
(answer input-lines)
