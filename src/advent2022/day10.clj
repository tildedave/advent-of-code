(ns advent2022.day10
  (:require [advent2022.utils :as utils]))

(def lines (utils/read-resource-lines "input/day10-example2.txt"))

(defn parse-line [line]
  (cond
    (= line "noop") [:noop]
    (.startsWith line "addx")
    [:addx (utils/parse-int (nth (.split line " ") 1))]))

;; takes a machine state (X register value + currently executing instructions)
;; and converts it into a new state.
;; this is sort of annoying because Clojure vectors don't support double-ended
;; operations so we convert the rest to a seq and then force is back into a
;; vector.  obviously this is bad :-(
(defn process-line [[cycle x-register executing x-states] program]
  ;; this is the list to reduce
  (let [now-executing (conj executing (case (first program)
                                        :noop [1 :noop]
                                        :addx [2 :addx (second program)]))
        [next-x next-executing]
        (let [[[cycles instr] & r] now-executing
              next-r (vec r)
              program (first now-executing)]
          (if (= cycles 1)
            ;; process it
            (case instr
              :noop [x-register next-r]
              :addx [(+ x-register (nth program 2)) next-r])
            ;; decrement and replace it as the first
            [x-register (vec (concat [(assoc program 0 (dec cycles))] r))]))]
    [(inc cycle) next-x next-executing (conj x-states x-register)]))

(concat [1] [2 3 4])
(conj [1 2 3 4] 10)

(let [[x & xs] [1 2 3 4]]
  (list x xs))
(def test-program (map parse-line lines))
(def full-program (concat test-program (repeat 100 [:noop])))



(count test-program)

(def beep (reduce process-line [1 1 [] []] full-program))

(nth (last beep) 220)

(let [[_ _ _ x-history] (reduce process-line [1 1 [] []] full-program)
      indexes (map #(+ % 20) (range 0 220 40))]
  (reduce + (map (fn [n] (* n (nth x-history n))) indexes)))

(nth 20 ())
result
(take 20 (last result))
(nth (last result) 21)
(nth (last result) 17)
(nth (nth result 3) 19)
(nth (nth result 3) 21)
(nth (nth result 3) 22)
(nth (nth result 3) 23)
(nth (nth result 3) 24)
