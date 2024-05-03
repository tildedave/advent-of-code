(ns advent2016.day25
  (:require [utils :as utils]
            [advent2016.assembunny :as assembunny]))

(defn out-signal [[program n state]]
  (if-let [[instr x] (get program n)]
    (if (= instr :out)
      (get state x)
      nil)
    nil))

(defn to-loop [program-seq]
;; day25 doesn't have a toggle so we'll just use state / n as the hash.
  (reduce (fn [[seen-states s] [program state n]]
            (if (contains? seen-states [state n])
              (reduced s)
              [(conj seen-states [state n]) (conj s [program state n])]))
          [#{} []]
          program-seq))

;; the sequence is length 12.
;; first character: number ^ 2
;; second character: number - 2 ^ 4

{10 (0 0 0 0 0 0 0 0 0 1 0 1)}
1 2 4 8 16 32 64 128 256

;; (+ 10 1 4 16 64 256)

;; our answer
(+ 10 2 8 32 128)

(->> {"a" 180}
     (assembunny/program-seq "2016/day25.txt")
     (to-loop)
     (map out-signal)
     (remove nil?))
