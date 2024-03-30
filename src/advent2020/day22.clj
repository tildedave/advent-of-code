(ns advent2020.day22
  (:require [utils :as utils]))

(utils/read-input "2020/day22.txt")
(defn parse-input [filename]
  (->> filename
       (format "2020/%s" filename)
       (utils/read-input)
       (utils/split-by "")
       (map rest)
       (map #(mapv utils/parse-int %))
       (map-indexed vector)
       (into {})))

(defn step [deck]
  (let [[deck1 deck2] [(get deck 0) (get deck 1)]
        [c1] deck1
        [c2] deck2]
    (cond
      (> c1 c2) {0 (-> (subvec deck1 1)
                       (conj c1)
                       (conj c2))
                 1 (subvec deck2 1)}
      (> c2 c1) {0 (subvec deck1 1)
                 1 (-> (subvec deck2 1)
                       (conj c2)
                       (conj c1))})))

(defn final-score [deck]
  (->> deck
       (reverse)
       (map-indexed vector)
       (map (fn [[n x]] (* (inc n) x)))
       (reduce +)))

(defn answer-part1 [filename]
  (reduce
   (fn [_ state]
     (cond
       (empty? (get state 0)) (reduced (final-score (get state 1)))
       (empty? (get state 1)) (reduced (final-score (get state 0)))
       :else nil))
   (iterate step (parse-input filename))))

(answer-part1 "day22-example.txt")
(answer-part1 "day22.txt")

;; OK step 2 has "recursive combat"

;; 1) if we have seen this state before in this game, player 1 wins
;; 2) otherwise, draw top card of each deck.
;; 3) if BOTH players have at least as many cards in their deck as the card
;; they just drew, we create a SUBGAME (recursive combat).
;; 4) otherwise, higher value card wins.

;; recursive combat: copy the cards they currently have, start a new game with
;; the above rules.

;; game state is now the current deck state and the list of SEEN board states.
;; we iterate through the game state until we have a winner... ?

(declare winner)

(defn step-p2 [[seen-states deck]]
  (let [[deck1 deck2] [(get deck 0) (get deck 1)]
        [c1] deck1
        [c2] deck2
        rest-deck1 (subvec deck1 1)
        rest-deck2 (subvec deck2 1)
        next-seen-state (conj seen-states deck)]
    [next-seen-state
     (cond
      ;; we've seen the state, p1 wins
      (contains? seen-states deck) {0 (-> rest-deck1
                                           (conj c1)
                                           (conj c2))
                                     1 rest-deck2}
      (and (<= c1 (count rest-deck1))
           (<= c2 (count rest-deck2)))
      (case (first (winner {0 rest-deck1 1 rest-deck2}))
        0 {0 (-> rest-deck1
                 (conj c1)
                 (conj c2))
           1 rest-deck2}
        1 {0 rest-deck1
           1 (-> rest-deck2
                 (conj c2)
                 (conj c1))})
      (> c1 c2) {0 (-> rest-deck1
                       (conj c1)
                       (conj c2))
                 1 rest-deck2}
      (> c2 c1) {0 rest-deck1
                 1 (-> rest-deck2
                       (conj c2)
                       (conj c1))})]))

(defn winner [deck-state]
  (reduce
    (fn [_ [_ state]]
      (cond
        (empty? (get state 0)) (reduced [1 (state 1)])
        (empty? (get state 1)) (reduced [0 (state 0)])
        :else nil))
    (iterate step-p2 [#{} deck-state])))

;; (take  (iterate step-p2 [#{} (parse-input "day22-example.txt")]))

(winner (parse-input "day22-example.txt"))
