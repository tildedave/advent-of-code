(ns advent2024.day2
  (:require [utils :as utils]))

(defn is-monotonic? [num-list comp]
  (boolean
   (reduce (fn [n1 n2]
            (if (comp n2 n1)
              (reduced false)
              n2))
           (first num-list)
           (rest num-list))))

(is-monotonic? [1 3 6 7 9] <)

(defn differences-ok? [num-list num-allowed-differences]
  (boolean
   (reduce
    (fn [n1 n2]
      (if (< 0 (abs (- n1 n2)) 4)
        n2
        (reduced false)))
    (first num-list)
    (rest num-list))))

(defn is-safe? [num-list]
  (and (differences-ok? num-list 0) (or (is-monotonic? num-list <) (is-monotonic? num-list >))))

(assert (is-safe? [7 6 4 2 1]))
(assert (not (is-safe? [1 2 7 8 9])))
(assert (not (is-safe? [9 7 6 2 1])))
(assert (not (is-safe? [1 3 2 4 5])))
(assert (not (is-safe? [8 6 4 4 1])))
(assert  (is-safe? [1 3 6 7 9]))

;; part 1
(->> (utils/read-input "2024/day2.txt")
     (map utils/parse-number-list)
     (filter is-safe?)
     (count))

(defn is-safe-part2? [num-list]
  (and (differences-ok? num-list 1) (or (is-monotonic? num-list <) (is-monotonic? num-list >))))

(assert (is-safe-part2? [7 6 4 2 1]))
(assert (not (is-safe-part2? [1 2 7 8 9])))
(assert (not (is-safe-part2? [9 7 6 2 1])))
(assert (is-safe-part2? [1 3 2 4 5]))
(assert (not (is-safe-part2? [8 6 4 4 1])))
(assert  (is-safe-part2? [1 3 6 7 9]))

;; part 1
(->> (utils/read-input "2024/day2.txt")
     (map utils/parse-number-list)
     (filter is-safe?)
     (count))
