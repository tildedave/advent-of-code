(ns advent2017.day14
  (:require [advent2017.day10 :refer [knot-hash]]
            [utils :as utils]))

;; probably a better way to do this but I'm tired
(defn hex-ch-to-binary [ch]
  (case ch
    \0 '(0 0 0 0)
    \1 '(0 0 0 1)
    \2 '(0 0 1 0)
    \3 '(0 0 1 1)
    \4 '(0 1 0 0)
    \5  '(0 1 0 1)
    \6  '(0 1 1 0)
    \7  '(0 1 1 1)
    \8  '(1 0 0 0)
    \9  '(1 0 0 1)
    \a '(1 0 1 0)
    \b  '(1 0 1 1)
    \c  '(1 1 0 0)
    \d  '(1 1 0 1)
    \e '(1 1 1 0)
    \f '(1 1 1 1)))

(->> (knot-hash "flqrgnkx-0")
     (seq)
     (map hex-ch-to-binary)
     (reduce concat))

(defn grid-coords [input]
  (reduce
   merge
  (for [n (range 0 128)]
    (->> (knot-hash (format "%s-%d" input n))
         (seq)
         (map hex-ch-to-binary)
         (reduce concat)
         (map-indexed (fn [m b] {[m n] b}))
         (reduce merge)))))

(defn answer [input]
  (->> input
       (grid-coords)
       (vals)
       (remove zero?)
       (count)))

(answer "flqrgnkx")
(answer (first(utils/read-input "2017/day14.txt")))
;; (count (remove zero? (vals (grid-coords "flqrgnkx"))))
