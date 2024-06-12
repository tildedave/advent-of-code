(ns advent2019.day2
  (:require [advent2019.intcode :as intcode]
            [utils :as utils]))

(intcode/step-program (intcode/parse-program "1,0,0,0,99"))

;; answer to part 1
(-> (intcode/parse-file "2019/day2.txt")
    (assoc-in [:program 1] 12)
    (assoc-in [:program 2] 2)
    (intcode/halting-state)
    (get-in [:program 0]))


;; answer to part 2.  kind of depends on us knowing
;; noun/verb has a max value.
;; an alternative would be to use some enumeration
;; of Z x Z, e.g. snake enumeration, so we could
;; use a truly infinite sequence.
(let [program (intcode/parse-file "2019/day2.txt")]
  (->>
   (for [noun (range 0 100)
        verb (range 0 100)]
    [noun verb])
   (filter (fn [[noun verb]]
    (-> program
        (assoc-in [:program 1] noun)
        (assoc-in [:program 2] verb)
        (intcode/halting-state)
        (get-in [:program 0])
        (= 19690720))))
   (first)
   ((fn [[noun verb]] (+ (* noun 100) verb)))))
