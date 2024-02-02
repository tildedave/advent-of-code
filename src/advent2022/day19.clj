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
    {:ore {:ore ore-cost}
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

;; I suppose greedy probably works here but we can start with A*
;; can actually use the heuristic to incentivize doing the right stuff (more
;; geode robots at an earlier time).

(use 'clojure.tools.trace)
(defn can-build? [blueprint {:keys [resources]} resource]
  (every?
   (fn [[resource num]] (>= (get resources resource 0) num))
   (blueprint resource)))

(can-build? blueprint {:resources {:ore 2}} :clay)

(defn collect-resources [resources robots]
  (into {}
        (map
         (fn [[resource num]]
           [resource (+ num (get robots resource 0))])
         resources)))

(defn next-states [blueprint state]
  (let [{:keys [time-left resources robots]} state
        resources (collect-resources resources robots)]
    (cond
      ;; we shouldn't ever get here, but just in case.
      (zero? time-left) (list)
      ;; no reason to build a robot in the last minute
      (= time-left 1) (list {:time-left (dec time-left)
                             :resources resources
                             :robots robots})
      :else
      (conj
       (->> '(:ore :clay :obsidian :geode)
            (filter (partial can-build? blueprint state))
            (map
             (fn [resource]
               {:time-left (dec time-left)
                :resources (spend-resources resources (blueprint resource))
                :robots (update robots resource (fnil inc 0))
                :spent-resources (blueprint resource)
                :built-robot resource})))
       {:time-left (dec time-left)
        :resources resources
        :robots robots}))))


(next-states blueprint {:time-left 22, :resources {:ore 2, :geode 0, :obsidian 0, :clay 0}, :robots {:ore 1}})

(collect-resources {:geode 2 :ore 1} {:geode 2 :ore 3})

;; (take 8 (iterate #(mapcat (fn [s] (next-states blueprint s)) %) [{:time-left 24
;;                                                                   :resources {:ore 0 :geode 0 :obsidian 0 :clay 0}
;;                                                                   :robots {:ore 1}}]))

(next-states blueprint {:time-left 100
                        :resources {:ore 100 :geode 100 :obsidian 100 :clay 100}
                        :robots {:ore 1 :geode 1 :obsidian 1 :clay 1}})

(next-states blueprint
             {:time-left 24
              :resources {:ore 0 :geode 0 :obsidian 0 :clay 0}
              :robots {:ore 1}})

(defn heuristic [state]
  ;; what is the format of the state?  obviously, time left, amount of
  ;; materials, what robots you have.
  ;; heuristic seems bugged now.
  (let [{:keys [resources robots]} state
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

;; if you have no geode robot and you build a geode robot
;; in this minute, you will collect (dec time-left) geodes.
;; if this can't beat our best score so far, no reason to
;; look at this node.

(defn should-cutoff [state best-so-far]
  (let [{:keys [time-left resources robots]} state]
    (or
     (= time-left 0)
     (and (= (get robots :geode 0) 0)
          (< time-left best-so-far)))))

;; this is BFS with a heuristic
(defn best-score [blueprint]
  (let [start {:time-left 25 ;; "minute 0", initial state
               :resources {:ore 0 :geode 0 :obsidian 0 :clay 0}
               :robots {:ore 1}}]
    (loop
     [[pqueue prev] [(priority-map start (heuristic start)) {}]
      best-so-far 0
      nodes 0]
      (cond (empty? pqueue) [best-so-far nodes prev]
       ;; we actually want the highest geode score here, but w/e at this point.
            (> nodes 1000000000) "cutoff" ;; just cutoff
            :else
            (let [[state] (peek pqueue)
                  {:keys [time-left resources robots]} state
                  next-best-so-far (if (= time-left 0)
                                (max best-so-far (resources :geode))
                                best-so-far)
                  best-so-far next-best-so-far
                  pqueue (pop pqueue)]
              (cond (should-cutoff state best-so-far)
                    (recur [pqueue prev] best-so-far (inc nodes))
                    :else
                    (recur
                     (reduce
                      (fn [[pqueue prev] neighbor]
                        (let [{:keys [time-left resources robots built-robot]} neighbor
                              has-geode-robot (contains? robots :geode)]
                 ;; use built-robot to cutoff things we don't want to explore
                          [(assoc pqueue neighbor 0)
                           (assoc prev neighbor state)]
               ;; 2) cutoff if there's no way to beat max-score-so-far in the
               ;; remaining time
                          ))
                      [pqueue prev]
                      (next-states blueprint state))
                     best-so-far
                     (inc nodes))))))))

(def blueprint2 "Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.")

(def prev (nth (best-score blueprint) 2))

(prev {:time-left 22, :resources {:ore 1, :geode 0, :obsidian 0, :clay 0}, :robots {:ore 1 :clay 1} :built-robot :clay})

(let [[score nodes prev] (best-score blueprint)
      best-node (first (filter #(= ((% :resources) :geode) score) (keys prev)))]
  (println "best score for blueprint" score "in" nodes "nodes")
  (loop [node best-node]
    (if (nil? node)
      (println "done")
      (do
        (println node)
        (recur (prev node)))))
  (println best-node))

;; (println "best score for blueprint" (best-score (parse-blueprint blueprint2)))
