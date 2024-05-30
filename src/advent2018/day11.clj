(ns advent2018.day11
  (:require [utils :as utils]))

(defn power [[x y] serial-num]
  (let [rack-id (+ x 10)]
    (-> (* rack-id y)
        (+ serial-num)
        (* rack-id)
        (mod 1000)
        (quot 100)
        (- 5))))

(power [3 5] 8)
(power [122 79] 57)
(power [217 196] 39)
(power [101 153] 71)

(defn square-at [[x y] n]
  (for [dx (range n)
        dy (range n)]
    [(+ x dx) (+ y dy)]))

(defn answer [serial-num]
  (let [cell-power (->>
                    (for [x (range 0 300)
                          y (range 0 300)]
                      {[x y] (power [x y] serial-num)})
                    (reduce merge))]
    (->>
     (for [n (range 2 25)]
       (->>
        (for [x (range 0 (- 300 n))
              y (range 0 (- 300 n))]
          [[x y n] (reduce + (map cell-power (square-at [x y] n)))])
        (sort-by second >)
        (first)))
     (sort-by second >)
     (first))))

;; (answer 18)
(answer (utils/parse-int (first (utils/read-input "2018/day11.txt"))))
