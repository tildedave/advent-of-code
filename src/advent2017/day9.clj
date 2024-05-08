(ns advent2017.day9
  (:require [utils :as utils]))

;; state of the reducer -
;; current group score
;; completed group scores
;; in garbage?

(defn step [[state ch-seq]]
  (if-let [[ch & ch-seq] ch-seq]
    (let [{:keys [current-group completed-groups in-garbage?]} state]
      (if in-garbage?
        (case ch
          \! [state (drop 1 ch-seq)]
          \> [(assoc state :in-garbage? false) ch-seq]
          [(update state :garbage-chars inc) ch-seq])
        (case ch
          \} [(-> state
                  (assoc :current-group (dec current-group))
                  (assoc :completed-groups (conj completed-groups current-group)))
              ch-seq]
          \{ [(-> state
                  (assoc :current-group (inc current-group)))
              ch-seq]
          \< [(-> state
                  (assoc :in-garbage? true))
              ch-seq]
          [state ch-seq]
          )))
    [state ch-seq]))

(defn initial-state []
  {:current-group 0 :completed-groups [] :in-garbage? false :garbage-chars 0})

(defn answer [s]
  (reduce
   (fn [_ [state ch-seq]]
     (if (nil? ch-seq)
       (reduced {:garbage-chars (:garbage-chars state)
                 :group-score (reduce + (:completed-groups state))})
       nil))
   (iterate step [(initial-state) (seq s)])))

(answer "{{{},{},{{}}}}")
(answer "{{<ab>},{<ab>},{<ab>},{<ab>}}")
(answer "{{<!!>},{<!!>},{<!!>},{<!!>}}")
(answer "{{<a!>},{<a!>},{<a!>},{<ab>}}")
(answer (first (utils/read-input "2017/day9.txt")))
