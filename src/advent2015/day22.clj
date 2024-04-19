(ns advent2015.day22
  (:require [utils :as utils]
            [grid :as grid]))

(def ^:dynamic part2? false)

;; so I guess this is an a* search
;; we'll use "mana cost" as the distance between two states.
;; we *always* have to cast a spell, otherwise we lose.
;; that makes this a little easier [no stand pat].

(def actions [:magic-missile :drain :shield :poison :recharge])
(def costs
  {:magic-missile 53
   :drain 73
   :shield 113
   :poison 173
   :recharge 229})

(def timers
  {:shield 6
   :poison 6
   :recharge 5})

(defn tick-effects [state]
  (reduce
   (fn [state effect]
     (let [n (get-in state [:effects effect])
           state (if (= n 1)
                   (utils/dissoc-in state [:effects effect])
                   (update-in state [:effects effect] dec))]
       (case effect
         :poison (update state :boss-hp #(- % 3))
         :recharge (update state :mana (partial + 101))
         state
         )))
   state
   (keys (:effects state))))

(defn boss-dead? [state]
  (<= (get state :boss-hp) 0))

(defn player-dead? [state]
  (<= (get state :hp) 0))

(defn process-boss-attack [state]
  (update state :hp #(let [armor (if (contains? (state :effects) :shield) 7 0)]
                       (- % (- (state :boss-damage) armor)))))

(defn next-actions [state]
  (let [{:keys [effects mana hp boss-hp]} state]
    (->> actions
         (remove (partial contains? effects))
         (remove #(< mana (costs %)))
         (map
          (fn [action]
            (let [state (-> state
                            (update :mana #(- % (costs action)))
                            (assoc :spent-mana (costs action))
                            (assoc :action action))]
              (case action
                :magic-missile
                (update state :boss-hp #(- % 4))
                :drain
                (-> state
                    (update :boss-hp #(- % 2))
                    (update :hp #(+ % 2)))
                (:shield :poison :recharge)
                (-> state
                    (update :effects #(assoc % action (timers action)))))))))))

;; from my time in the advent-of-code A* salt mines, I know it is
;; generally easier to just process everything and then go to the next
;; action point.

(defn death-check [state]
  (cond
    (get state :result) state
    (player-dead? state) (assoc state :result :player-dead)
    (boss-dead? state) (assoc state :result :boss-dead)
    :else state
    ))

(defn hard-mode [state]
  (if part2?
    (-> state
      (update :hp dec)
      (death-check))
    state))

(defn starting-state [filename]
  (let [state (merge
               {:hp 50 :mana 500}
               (->> (utils/read-input filename)
                    (map #(second (.split % ": ")))
                    (map utils/parse-int)
                    (#(hash-map :boss-hp (first %) :boss-damage (second %)))))]
    (if part2?
      (hard-mode state)
      state)))

(starting-state "2015/day22.txt")

(defn neighbor-states [state]
  (if (:result state)
    (list)
    (let [action-states (next-actions state)]
      (if (empty? action-states)
        (list (assoc state :result :player-dead))
      ;; OK for each of these states, run through it to the next turn, have the
      ;; boss attack, that kind of thing, tick all effects, then that is it.
        (map
         (fn [state]
           (-> state
               (tick-effects)
               (death-check)
               (process-boss-attack)
               (#((if part2? hard-mode identity) %))
               (death-check)
               (tick-effects)))
         action-states)))))

(neighbor-states (starting-state "2015/day22.txt"))


(def example1-state {:hp 10 :mana 250 :boss-hp 13 :boss-damage 8})

;; looks good
(-> example1-state
    (neighbor-states)
    (nth 3)
    (neighbor-states))

(def example2-state {:hp 10 :mana 250 :boss-hp 14 :boss-damage 8})

;; also looking good
(-> example2-state
    (neighbor-states)
    (nth 4)
    (neighbor-states)
    (nth 2)
    (neighbor-states)
    (nth 1)
    (neighbor-states)
    (nth 2)
    (neighbor-states)
)

(grid/a*-search
 example1-state
 (fn [{:keys [result]}] (= result :boss-dead))
 neighbor-states
 (fn [{:keys [result boss-hp hp]}]
   (case result
     :boss-dead -100000
     :player-dead Integer/MAX_VALUE
     (+ boss-hp hp)))
 (fn [_ {:keys [spent-mana]}] spent-mana))

(grid/a*-search
 example2-state
 (fn [{:keys [result]}] (= result :boss-dead))
 neighbor-states
 (fn [{:keys [result boss-hp hp]}]
   (case result
     :boss-dead -100000
     :player-dead Integer/MAX_VALUE
     (+ boss-hp hp)))
 (fn [_ {:keys [spent-mana]}] spent-mana))


(neighbor-states {:hp 7, :mana 31, :boss-hp 6, :boss-damage 8, :spent-mana 53, :action :magic-missile})

(starting-state "2015/day22.txt")

(defn answer-part1 [filename]
  (grid/a*-search
   (starting-state filename)
   (fn [{:keys [result]}] (= result :boss-dead))
   neighbor-states
   (fn [{:keys [result boss-hp hp]}]
     (case result
       :boss-dead -100000
       :player-dead Integer/MAX_VALUE
       (+ boss-hp hp)))
   (fn [_ {:keys [spent-mana]}] spent-mana)))

(defn answer-part2 [filename]
  (binding [part2? true]
    (answer-part1 filename)))

(answer-part2  "2015/day22.txt")
