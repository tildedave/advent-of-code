(ns advent2022.day19
  (:require [advent2022.utils :as utils]
            [clojure.data.priority-map :refer [priority-map]]))

(def blueprint-re #"Blueprint \d+:\s+Each ore robot costs (\d+) ore.\s+Each clay robot costs (\d)+ ore.\s+Each obsidian robot costs (\d+) ore and (\d+) clay.\s+Each geode robot costs (\d+) ore and (\d+) obsidian.")

(def test-str "
Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.
")

(defn parse-blueprint [robot-string]
  (let [[_ & parts] (re-matches blueprint-re (.replaceAll robot-string "\n" ""))
        [ore-cost clay-ore-cost
         obs-ore-cost obs-clay-cost
         geode-ore-cost geode-obs-cost] (map utils/parse-int parts)]
    {:ore ore-cost
     :clay {:ore clay-ore-cost}
     :obsidian {:ore obs-ore-cost :clay obs-clay-cost}
     :geode {:ore geode-ore-cost :obsidian geode-obs-cost}}))

(defn can-acquire? [resource robots]
  (contains? robots resource))

(defn time-to-acquire [resource resource-amount robots]
  ;; this is blueprint agnostic, e.g. if you have 2 ore robots and want 8 ore
  ;; this is 4 seconds.
  ;; returns time plus any surplus resource
  (let [t (quot resource-amount (robots resource))
        r (mod resource-amount (robots resource))]
    (if (= r 0) [t 0] [(inc t) {resource r}])))

(time-to-acquire :ore 4 {:ore 3})
(def blueprint (parse-blueprint test-str))

(blueprint :clay)

(defn spend-resources [resources m]
  (into {} (map (fn [[k v]] [k (- v (get m k 0))]) resources)))

(spend-resources  {:clay 5 :geode 12}  {:clay 2})

;; I suppose greedy probably works here but we can start with A*
;; can actually use the heuristic to incentivize doing the right stuff (more
;; geode robots at an earlier time).

(defn can-build? [blueprint [_ resources _ :as state] resource]
  (every?
   (fn [[resource num]] (>= (get resources resource 0) num))
   (blueprint resource)))


(blueprint :clay)

(can-build? blueprint [30 {:ore 1} {:ore 1}] :clay)

(let [state [30 {:ore 0} {:ore 1}]
      [time-left resources robots] state]
  (->> '(:clay :obsidian)
       (filter (partial can-build? blueprint state))))

(defn collect-resources [resources robots]
  (into {}
        (map
         (fn [[resource num]]
           [resource (+ num (get robots resource 0))])
         resources)))

(defn next-states [blueprint state]
  (let [[time-left resources robots] state
        resources (collect-resources resources robots)]
    (if
     (zero? time-left) (list)
     (conj
      (->> '(:clay :obsidian :geode)
           (filter (partial can-build? blueprint state))
           (map
            (fn [resource]
              [(dec time-left)
               (spend-resources resources (blueprint resource))
               (update robots resource (fnil inc 0))])))
      [(dec time-left) resources robots]))))

(take 2 (iterate #(map (fn [state] (next-states blueprint state)) %) [[30 {} {:ore 1}]]))


(next-states blueprint [29 {} {:ore 1}])

(defn heuristic [state]
  ;; what is the format of the state?  obviously, time left, amount of
  ;; materials, what robots you have.
  (let [[time-left resources robots] state
        score 0]
    (+ score
       ;; feels more natural to list the things that we like
       ;; vs the things we don't like for the min heap.
       (* (get resources :geode 0) -100)
       (* (get resources :obsidian 0) -5)
       (* (get resources :clay 0) -1)
       (* (get robots :geode 0) -20)
       (* (get robots :obsidian 0) -5)
       (* (get robots :clay 0) -3))))

(peek (priority-map "bob" -1 "joe" 3))

(defn best-score [blueprint]
  (let [start [30 {:ore 0 :geode 0 :obsidian 0 :clay 0} {:ore 1}]]
    (loop
     [[open-set goal-score] [(priority-map start (heuristic start)) {start 0} ]
      nodes 0]
     (cond (empty? open-set)
       (reduce max
               (map
                (fn [[_ resources]] (resources :geode))
                (keys goal-score)))
       ;; we actually want the highest geode score here, but w/e at this point.
       (> nodes 100000000) "cutoff" ;; just cutoff
       :else
       (let [[state] (peek open-set)
             open-set (pop open-set)]
            ;;  (println "state" state)
        ;;  (println "next-states" blueprint state (next-states blueprint state))
         (recur
          (reduce
           (fn [[open-set goal-score] neighbor]
             (let [tentative-gscore (goal-score state)]
               (if (< tentative-gscore (get goal-score neighbor Integer/MAX_VALUE))
                 [(assoc open-set neighbor (+ tentative-gscore (heuristic neighbor)))
                  (assoc goal-score neighbor tentative-gscore)]
                 [open-set goal-score])))
           [open-set goal-score]
           (next-states blueprint state))
          (inc nodes)))))))

(println "best score for blueprint" (best-score blueprint))
