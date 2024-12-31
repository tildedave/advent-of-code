(ns advent2021.day21
  (:require [utils :as utils]))


(def ^:dynamic part2? false)

(def player-re #"^Player (\d+) starting position: (\d+)")

(defn parse-input [filename]
  (let [positions (->> (utils/read-input filename)
       (map (partial re-matches player-re))
       (map rest)
       (map #(mapv utils/parse-int %))
       (into {}))]
    {:positions positions
     :scores {1 0 2 0}
     :player 1}))

(def deterministic-die (cycle (range 1 101)))

(defn advance-space [current-space increment]
  (-> current-space
      (dec)
      (+ increment)
      (mod 10)
      (inc)))

(defn step [state]
  (let [player (state :player)
        die (state :die)
        roll-total (->> die (take 3) (reduce +))
        next-space (advance-space roll-total (get-in state [:positions player]))]
    (-> state
         (assoc-in [:positions player] next-space)
         (update-in [:scores player] (partial + next-space))
         (assoc :player (case player 1 2 2 1))
         (assoc :die (drop 3 die))
         (update :num-rolls (partial + 3)))))

(defn is-over? [state]
  (let [player (state :player)
        other-player (case player 1 2 2 1)]
    (>= (get-in state [:scores other-player])
        (if part2? 21 1000))))

(defn game-seq [filename]
  (iterate step (-> (parse-input filename)
                    (assoc :die deterministic-die)
                    (assoc :num-rolls 0))))

;; (map first (game-seq "day21-example.txt"))

(nth (game-seq "day21-example.txt") 2)

(defn final-score [state]
  (let [player (state :player)
        n (state :num-rolls)]
    (* (get-in state [:scores player]) n)))

(defn answer-part1 [filename]
  (->> (game-seq filename)
       (reduce #(if (is-over? %1) (reduced %1) %2))
       (final-score)))

(answer-part1 "day21-example.txt")
(answer-part1 "day21.txt")

;; for part 2, I'm assuming this can be solved via a similar
;; approach to the fish spawning problem.  enumerate all
;; states in a map, for each one that's not terminal,
;; determine next states, multiply totals, go until the end.

;; everything kind of carries forward except the die is no longer part of
;; the state.

;; OK, we actually have to roll 3 times.
;; that's where the state increase comes from.
(defn next-states [state]
  (for [roll1 (range 1 4)
        roll2 (range 1 4)
        roll3 (range 1 4)]
    (let [roll-total (+ roll1 roll2 roll3) player (:player state)
          next-space (advance-space roll-total (get-in state [:positions player]))]
      (-> state
           (assoc-in [:positions player] next-space)
           (update-in [:scores player] (partial + next-space))
          (assoc-in [:player] (case player 1 2 2 1))))))

(defn initial-state [filename]
  {(assoc (parse-input filename) :player 1) 1})

(defn tick-states [all-states]
  ;; find the terminal ones, find the non-terminal ones,
  ;; if we're hit all terminal ones, do nothing, etc
  (let [state-grouping (group-by #(is-over? (first %)) all-states)
        terminal-states (state-grouping true)
        nonterminal-states (state-grouping false)]
    (->> nonterminal-states
         (mapcat (fn [[state count]]
                (for [next-state (next-states state)]
                  {next-state count})))
         (apply merge-with +)
         (merge-with + (into {} terminal-states))
  )))

(defn game-seq-part2 [filename]
  (iterate tick-states (initial-state filename)))

(defn answer-part2 [filename]
  (binding [part2? true]
    (->> (game-seq-part2 filename)
         (reduce #(if (every? is-over? (keys %1)) (reduced %1) %2))
         (group-by #((first %) :player))
         (#(update-vals % (fn [l] (reduce + (map second l)))))
         (vals)
         (sort >)
         (first))))

(answer-part2 "day21-example.txt")
(answer-part2 "day21.txt")
