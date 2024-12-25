(ns advent2024.day25
  (:require
    [utils :as utils]
    [grid :as grid]))

(defn is-lock? [grid]
  (every? (partial = \#) (first grid)))

(defn is-key? [grid]
  (every? (partial = \#) (last grid)))

;; easy
(filter is-key? (map grid/parse (utils/split-by "" (utils/read-input "2024/day25.txt"))))

(defn heights [grid]
  (let [[xmax ymax] (grid/bounds grid)]
  (for [x (range xmax)]
    (dec (count
     (for [y (if (is-key? grid)
              (range (dec ymax) -1 -1)
              (range 0 ymax))
          :while (= (grid/at grid [x y]) \#)]
      [x y]))))))
