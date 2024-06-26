(ns advent2016.day11
  (:require [utils :as utils]
            [clojure.set :as set]
            [graph :as graph]))

;; so this is a good problem for A* search
;; state representation is {:elevator X, floor {:microchips :generators}}
;; something like that.
;; we just need to define a heuristic function and a neighbor function.

(defn parse-line [n line]
  (let [[_ contents] (re-matches #"^.* contains (.+)\.$" line)]
    {(inc n)
     (if (= contents "nothing relevant")
      {:microchips #{} :generators #{}}
      (->
       (->> (.split #",?( and)? a " contents)
            (map #(.replaceAll % "a " ""))
            (group-by #(second (.split % " "))))
       (update-keys (fn [k] (case k "generator" :generators "microchip" :microchips)))
       (update-vals (fn [v-list] (set (map (fn [s] (first (.split s " |-"))) v-list))))))
    }))

(defn initial-state [lines]
  (assoc (reduce merge (map-indexed parse-line lines))
         :elevator 1))
    ;;   (assoc :elevator 1)))

(initial-state (utils/read-input "2016/day11-example.txt"))

;; so neighbor state is elevator goes up or elevator goes down.
;; we always have to take something with us.
;; we need a function that indicates for a floor state, if it's legal.

;; a floor state is legal if:
;; for every microchip, either its generator is there, or there are no other
;; generators.
(defn floor-legal? [{:keys [microchips generators]}]
  (reduce
   (fn [acc microchip]
     (if (or (contains? generators microchip) (empty? (disj generators microchip)))
       acc
       (reduced false)))
   true
   microchips))

(defn is-legal? [state]
  (->> (keys state)
       (filter number?)
       (every? #(floor-legal? (state %)))))

(floor-legal? ((initial-state (utils/read-input "2016/day11-example.txt")) 2))

(is-legal? (initial-state (utils/read-input "2016/day11-example.txt")))

;; ok now, neighbors of a position are those that move one or two things.
;; and we filter for legality.

(defn move [state level next-level kw thing]
  (-> state
      (update-in [level kw] (fnil #(disj % thing) #{}))
      (update-in [next-level kw] (fnil #(conj % thing) #{}))))

(defn move-up [state level kw thing]
  (move state level (inc level) kw thing))

(defn move-down [state level kw thing]
  (move state level (dec level) kw thing))

(move-up
 (initial-state (utils/read-input "2016/day11-example.txt"))
 1
 :microchips
 "hydrogen")

(defn next-state [state level next-level stuff-to-move]
  (reduce
   (fn [state [kw thing]]
     (move state level next-level kw thing))
   (assoc state :elevator next-level)
   stuff-to-move))


(defn neighbors [state]
  (let [level (state :elevator)
        current-floor-contents (state level)
        move-possibilities (->> (utils/combinations-up-to
                                 (concat
                                  (for [microchip (:microchips current-floor-contents)]
                                    [:microchips microchip])
                                  (for [generator (:generators current-floor-contents)]
                                    [:generators generator])) 2)
                                (remove empty?))]
    (->>
     (concat
      (if (= level 1) '() (map (partial next-state state level (dec level)) move-possibilities))
      (if (= level 4) '() (map (partial next-state state level (inc level)) move-possibilities)))
     (filter is-legal?))))

(->> (initial-state (utils/read-input "2016/day11-example.txt"))
     (neighbors)
     (first)
     (neighbors))

;; neighbors looks good now.

(vals ((initial-state (utils/read-input "2016/day11-example.txt")) 4))

(defn is-goal? [state]
  (->>
   (for [n (range 1 4)]
    (vals (state n)))
   (flatten)
   (reduce set/union)
   (empty?)))

(defn heuristic [state]
  (reduce *
          (for [n (range 1 4)]
    (* (- n) (count (reduce set/union (vals (state n))))))))

(defn state-hash [state]
  (assoc
   (reduce merge
   (for [n (range 1 5)]
     (let [{:keys [generators microchips]} (state n)
           pairs (set/intersection generators microchips)
           unpaired-microchips (set/difference microchips pairs)
           unpaired-generators (set/difference generators pairs)]
       {n (concat
           (repeat (count unpaired-generators) :generator)
           (repeat (count unpaired-microchips) :microchip)
           (repeat (count pairs) :pair))}))
   )
   :elevator (state :elevator)))

(state-hash (initial-state (utils/read-input "2016/day11-example.txt")))


(graph/a*-search
 (initial-state (utils/read-input "2016/day11-example.txt"))
 is-goal?
 neighbors
 heuristic
 (fn [_ _] 1)
 state-hash)

(defn answer-part1 []
  (first
   (graph/a*-search
    (initial-state (utils/read-input "2016/day11.txt"))
    is-goal?
    neighbors
    heuristic
    (fn [_ _] 1)
    state-hash)))


;; (heuristic (initial-state (utils/read-input "2016/day11-example.txt")))
(answer-part1)


(defn answer-part2 []
  (first
   (graph/a*-search
   (-> (initial-state (utils/read-input "2016/day11.txt"))
       (update-in [1 :generators] #(conj % "elerium"))
       (update-in [1 :microchips] #(conj % "elerium"))
       (update-in [1 :generators] #(conj % "dilithium"))
       (update-in [1 :microchips] #(conj % "dilithium")))
   is-goal?
   neighbors
   heuristic
   (fn [_ _] 1)
   state-hash)))

(println (answer-part2))
