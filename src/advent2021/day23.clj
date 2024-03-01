(ns advent2021.day23
  (:require [advent2021.utils :as utils]
            [advent2021.grid :as grid]
            [clojure.core.match :refer [match]]))

(set! *warn-on-reflection* true)

(def amber-column 3)
(def bronze-column 5)
(def copper-column 7)
(def desert-column 9)

(def positions
  {:amber 3
   :bronze 5
   :copper 7
   :desert 9})

(def all-rooms [:amber :bronze :copper :desert])

(defn parse-input [filename]
  (let [grid (-> filename
                 (utils/read-input)
                 (grid/parse identity))]
    (into {:hallway {}}
          (map (fn [color]
                 (->> grid
                      (map #(get % (positions color)))
                      (filterv #(contains? #{\A \B \C \D} %))
                      (hash-map color)))
               all-rooms))))

(parse-input "day23-example.txt")

(def move-cost
  {:amber 1
   :bronze 10
   :copper 100
   :desert 1000})

(utils/read-input "day23-example.txt")
(def ^:dynamic part2? false)


(defn steps-to-hallway [state room]
  (let [room-size (if part2? 4 2)]
    (inc (- room-size (count (state room))))))


(steps-to-hallway {:bronze [\B \C]} :bronze)

(defn steps-from-hallway [state room]
  (let [room-size (if part2? 4 2)]
    (- room-size (count (state room)))))

(defn steps-in-hallway [state hallway-num1 hallway-num2]
  (abs (- hallway-num1 hallway-num2)))

(defn num-steps-room-to-hallway [state room hallway-num]
  ;; we need to know how many steps out of the room and
  ;; into the hallway (it's either 1 or 2 and it's based on
  ;; if the first member of the room is nil)
  ;; dec since the columns have the +1 wall in it.
  (let [hallway-entrance (dec (positions room))]
    (+ (steps-to-hallway state room)
       (steps-in-hallway state hallway-entrance hallway-num))))

(defn hallway-nums [state room]
  (let [hallway (state :hallway)
        start (dec (positions room))
        walk-in-direction
        (fn [f]
          (loop [i start
                 n 0
                 valid-nums (list)]
            (cond
              (< i 0) valid-nums
              (> i 10) valid-nums
              (not (nil? (get hallway i nil))) valid-nums
              :else (recur (f i) (inc n) (if (not= n 0) (conj valid-nums i) valid-nums)))))]
    (sort (concat (walk-in-direction inc) (walk-in-direction dec)))))

(hallway-nums (parse-input "day23-example.txt") :amber)

(defn num-steps-room-to-room [state room1 room2]
  (+ (steps-to-hallway state room1)
     ;; technically we need to (dec) both for these to be right but
     ;; the answer is the same.
     (steps-in-hallway state (positions room1) (positions room2))
     (steps-from-hallway state room2)))

(num-steps-room-to-room {:amber [\B \A] :bronze [nil \A]} :amber :bronze)

(defn num-steps-to-room-from-hallway [state room hallway-num]
  (+
   (steps-in-hallway state hallway-num (dec (positions room)))
   (steps-from-hallway state room)))

(def goal
  (let [room-size (if part2? 4 2)]
    {:amber (vec (repeat room-size \A))
     :bronze (vec (repeat room-size \B))
     :copper (vec (repeat room-size \C))
     :desert (vec (repeat room-size \D))}))

;; so it seems like the hallway is the same between the example
;; and puzzle input; I can just hardcode the size.

;; we'll just use A* search.
;; we'll use an intermediate representation.

(defn is-complete? [state room]
  (let [room-size (if part2? 4 2)]
    (case room
      :amber (= (state room) (repeat room-size \A))
      :bronze (= (state room) (repeat room-size \B))
      :copper (= (state room) (repeat room-size \C))
      :desert (= (state room) (repeat room-size \D)))))

(defn amphipod-for-room [room]
  (case room
    :amber \A
    :bronze \B
    :copper \C
    :desert \D))

(defn needs-move? [state room]
  (if (is-complete? state room)
    false
    (not (every? (partial = (amphipod-for-room room)) (state room)))))

;; (match [room (state room)]
;;           [:amber [\A]] false
;;           [:amber []] false
;;           [:bronze [\B]] false
;;           [:bronze []] false
;;           [:copper [\C]] false
;;           [:copper []] false
;;           [:desert [\D]] false
;;           [:desert []] false
;;           :else true)))
    ;; (case room
    ;;   :amber (every? (partial = \A) (state room))
    ;;   :bronze (every? (partial = \B) (state room))
    ;;   :copper (every? (partial = \C) (state room))
    ;;   :desert (every? (partial = \D) (state room)))))

(defn room-for-amphipod [amphipod]
  (case amphipod
    \A :amber
    \B :bronze
    \C :copper
    \D :desert))

(range 5 3)

(defn range-inclusive [m n]
  (range m (inc n)))

;; encapsulate both "can" and "should"
(defn can-move-to-room-from-hallway? [state hallway-num]
  (let [room (room-for-amphipod (get-in state [:hallway hallway-num]))
        dest-hallway (dec (positions room))
        hallway-spaces (->> (sort [dest-hallway hallway-num])
                            (apply range-inclusive)
                            (remove (partial = hallway-num)))]
  ;; we don't want to move into a room that itself would need a move
    (if (needs-move? state room)
      false
      (every? #(nil? (get-in state [:hallway %] nil)) hallway-spaces))))

(defn can-move-to-room-from-room? [state source-room]
  (if-let [thing-moving (get-in state [source-room 0])]
    (let [source-hallway (dec (positions source-room))
          dest-room (room-for-amphipod thing-moving)
          dest-hallway (dec (positions dest-room))
          hallway-spaces (->> (sort [source-hallway dest-hallway])
                              (apply range-inclusive))]
      (if (needs-move? state dest-room)
        false
        (every? #(nil? (get-in state [:hallway %] nil)) hallway-spaces)))
    false))

(def beep {:hallway {} :amber [\C \C] :copper []})
(can-move-to-room-from-room? beep :amber)
(needs-move? beep :copper)

(can-move-to-room-from-hallway?
 {:amber [nil \A]
  :hallway {2 \A}}
 2)

(defn hallway-moves [state]
  ;; for every element of the hallway, find its destination room.
  ;; if it can move to its actual home, do it.
  ;; count number of steps, multiply by cost, then the next state is
  ;; that updated state with a cost on the tag.
  (remove
   nil?
   (for [[hallway-num amphipod] (state :hallway)]
     (let [room (room-for-amphipod amphipod)]
       ;; "should move" is also checked by can-move.
       (if (can-move-to-room-from-hallway? state hallway-num)
         (let [steps (num-steps-to-room-from-hallway state room  hallway-num)]
           (-> state
               (assoc :cost (* steps (move-cost room)))
               (utils/dissoc-in [:hallway hallway-num])
               (update room #(into [amphipod] %))))
         nil)))))

(hallway-moves {:hallway {0 \A} :amber [:bronze]})

(defn room-to-room-moves [state room]
  (if (can-move-to-room-from-room? state room)
    (let [amphipod (first (state room))
          dest-room (room-for-amphipod amphipod)
          steps (num-steps-room-to-room state room dest-room)]
      (-> state
          (assoc :cost (* steps (move-cost dest-room)))
          (update dest-room #(into [amphipod] %))
          (update room #(subvec % 1))))
    nil))

(defn room-to-hallway-moves [state room]
  (if-let [amphipod (first (state room))]
    (for [hallway-num (hallway-nums state room)]
      (let [cost (move-cost (room-for-amphipod amphipod))
            steps (num-steps-room-to-hallway state room hallway-num)]
        (-> state
            (assoc :cost (* steps cost))
            (update room #(subvec % 1))
            (assoc-in [:hallway hallway-num] amphipod))))
    nil))

;; moves from room to hallway and room to room.
(defn room-moves [state]
  ;; for each room that "needs a move", move either to the hallway or if they can, to the destination room.
  (remove nil?
          (flatten
           (for [room all-rooms]
             (if (needs-move? state room)
       ;; this is us being greedy and moving directly into
       ;; a room if it's available (vs into the hallway)
       ;; this feels like it must be a safe "greedy choice".
               (if-let [r2r-moves (room-to-room-moves state room)]
                 r2r-moves
                 (room-to-hallway-moves state room))
        ;; we also need to generate hallway moves.
               nil)))))

(hallway-moves (first (hallway-moves {:hallway {10 \D, 5 \D}, :amber [\A \A], :bronze [\B \B], :copper [\C \C], :desert [], :cost 200})))

(def test-state {:amber [\C] :bronze [\A] :copper []})
(hallway-nums test-state :bronze)

(can-move-to-room-from-room? test-state :amber)
(room-to-room-moves test-state :amber)
(room-moves test-state)
(defn neighbors [state]
  (concat (room-moves state) (hallway-moves state)))

(defn heuristic [state]
  ;; let's penalize, for each amphipod, how many steps to get
  ;; to the mouth of their dest room
  (->> (for [room all-rooms]
         (for [amphipod (state room)]
           (let [cost (move-cost room)
                 dest-room (room-for-amphipod amphipod)]
             (* (steps-in-hallway
                 state
                 (positions room) (positions dest-room))
                cost))))
       (flatten)
       (reduce +)))

(defn answer-part1 [filename]
  (grid/a*-search
   (parse-input filename)
   (fn [state] (= (dissoc state :cost) goal))
   neighbors
   heuristic
   (fn [_ neighbor] (neighbor :cost))
   (fn [state] (dissoc state :cost))))

(time (answer-part1 "day23-example.txt"))
;; 11 seconds, meh
(time (answer-part1 "day23.txt"))

;; we'll probably be able to crush part2 without changing much.
