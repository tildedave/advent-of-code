(ns advent2022.day19
  (:require [advent2022.utils :as utils]
            [clojure.data.priority-map :refer [priority-map]]))

(def blueprint-re #"Blueprint (\d+):\s+Each ore robot costs (\d+) ore.\s+Each clay robot costs (\d)+ ore.\s+Each obsidian robot costs (\d+) ore and (\d+) clay.\s+Each geode robot costs (\d+) ore and (\d+) obsidian.")

(def test-str "
Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.
")

(re-matches blueprint-re (.replaceAll test-str "\n" ""))

(defn parse-blueprint [robot-string]
  (let [[_ & parts] (re-matches blueprint-re (.replaceAll robot-string "\n" ""))
        [blueprint-num
         ore-cost clay-ore-cost
         obs-ore-cost obs-clay-cost
         geode-ore-cost geode-obs-cost] (map utils/parse-int parts)]
    {:number blueprint-num
     :ore {:ore ore-cost}
     :clay {:ore clay-ore-cost}
     :obsidian {:ore obs-ore-cost :clay obs-clay-cost}
     :geode {:ore geode-ore-cost :obsidian geode-obs-cost}}))

(defn can-acquire? [resource robots]
  (contains? robots resource))

(defn quot-round-up [n m]
  (let [x (quot n m)
        r (mod n m)]
    (if (= r 0) x (inc x))))

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

(defn spend-resources [state m]
  (let [{:keys [resources]} state]
    (assoc state :resources
           (into {} (map (fn [[k v]] [k (- v (get m k 0))]) resources)))))

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

;; next states will be build each robot (perhaps waiting several minutes)
;; OR stand pat until the end.

(defn transition [state t]
  (let [{:keys [resources robots]} state]
    ;; for each robot, add * t resource resources.
    (-> state
        (update :time-left #(- % t))
        (assoc :resources
               (merge-with
                +
                resources
                (into {} (map (fn [[resource num]]
                                [resource (* t num)])
                              robots)))))))

(defn stand-pat [state]
  (transition state (state :time-left)))

(def rich-state {:time-left 20 :resources {:ore 100 :obsidian 100 :clay 100 :geode 100} :robots {}})

(get-in rich-state [:resources :geode])
(use 'clojure.tools.trace)

(defn time-to-build [blueprint state resource]
  (let [{:keys [robots resources]} state]
    (let [needed (->> (blueprint resource)
                      (map (fn [[r v]] [r (- v (get resources r 0))]))
                      (filter (fn [[_ v]] (> v 0))))]
      (if (empty? needed) 0
          ;; rate for each one
          (->> needed
               ;; we don't check that this is possible
               (map (fn [[r v]] (quot-round-up v (get robots r 0))))
               (reduce max))))))

(time-to-build blueprint rich-state :geode)

    ;; ;; for each dependency, max of incoming rates.
    ;; ;; OK, needs to include our current stockpile.
    ;; (reduce
    ;;  max
    ;;  (map #(quot-round-up
    ;;         (- (get-in blueprint [resource %]) (get resources % 0))
    ;;         (get robots % 0))
    ;;       (keys (blueprint resource))))))

;; (use 'clojure.tools.trace)
(get {:ore 2} :ore)
(get-in blueprint [:clay :ore])

(time-to-build blueprint {:resources {:ore 2} :robots {:ore 1}} :clay)

(defn can-ever-build? [blueprint state resource]
  (let [{:keys [time-left resources robots]} state]
    (and (every? #(> (get robots % 0) 0) (keys (blueprint resource)))
         ;; I think this needs to be <, potentially needs to be even more
         ;; aggressive.  if I can build a geode robot with 1 second left, it
         ;; should be as if I can't build it.
         ;; being more aggressive is managed by (dec time-left)
         (< (time-to-build blueprint state resource) (dec time-left)))))

(def better-state {:time-left 24
                   :robots {:obsidian 1 :ore 1}})
(def out-of-time-state {:time-left 7
                        :robots {:obsidian 1 :ore 1}})
(can-ever-build? blueprint better-state :geode)
(can-ever-build? blueprint out-of-time-state :geode)
(stand-pat better-state) ;; should be 24 24
(transition better-state 0) ;; should be 24 24

;; this is off by one, the minute spend collecting the resources is not taken
;; into account.
;; transition zooms forward to the START of the time.
;; we then build the robot.
(defn build-robot [blueprint state resource]
  (let [t (time-to-build blueprint state resource)
        ;; head forward to the start of a time when we have the resources to
        ;; build the robot.
        next-state (transition state t)]
    (-> next-state
        ;; "tick" the state; we need to do this first because we don't want to
        ;; count the being-created robot for collection.
        (transition 1)
        (update-in [:robots resource] (fnil inc 0))
        (spend-resources (blueprint resource))
        (assoc :built-robot resource))))

;; a robot is useless if we already have enough resources coming in a turn to
;; build any other type of robot.

(def max-robots
  (memoize
   (fn [blueprint]
     (apply merge
     (for [resource '(:obsidian :clay :ore)]
       {resource
        (->>
         (dissoc blueprint :number)
         (map (fn [[k x]] (x resource)))
         (remove nil?)
         (reduce max))})))))

(map (fn [x] x) (dissoc blueprint :number))

(map identity blueprint)
(max-robots blueprint)

(defn robot-useless? [blueprint {:keys [time-left resources robots built-robot]}]
  (cond
    (= built-robot :geode) false
    (= time-left 1) (not= built-robot :geode)
    ;; since the robot-useless filter is done AFTER the robot is built,
    ;; this has to be >
    :else (->> (> (get robots built-robot 0)
                   ((max-robots blueprint) built-robot)))))

(defn next-states-better [blueprint state]
  (let [{:keys [time-left resources robots]} state]
    (cond
      (= time-left 1) (list (stand-pat state))
      ;; this
      (can-build? blueprint state :geode) (list (build-robot blueprint state :geode))
      ;; otherwise, decide what to do next.
      ;; we prioritize building robots in order of priority.
      :else
      (let [next (->> '(:geode :obsidian :clay :ore)
                      (filter (partial can-ever-build? blueprint state))
                      (map (partial build-robot blueprint state))
                      (remove (partial robot-useless? blueprint))
                      )]
        (if (empty? next) (list (stand-pat state)) next)))))

;; if you have no geode robot and you build a geode robot
;; in this minute, you will collect (dec time-left) geodes.
;; if this can't beat our best score so far, no reason to
;; look at this node.

(def triangle-seq (mapv #(* % (dec %)) (range 0 33)))

(defn max-geode-potential
  "max number of geodes we could potentially get from this state"
  [state]
  ;; we have to be careful because we might be able to build a geode robot
  ;; this turn, a geode robot next turn, etc.
  ;; we'd have to understand resource intake to be able to do the calculation
  ;; intelligently.
  ;; https://old.reddit.com/r/adventofcode/comments/zujwgo/comment/j1jobsq
  (let [{:keys [time-left resources robots]} state
        ;; assume we could build a geode robot each turn
        geodes (get resources :geode 0)
        geode-robots (get robots :geode 0)]
    (+ geodes (* geode-robots time-left) (nth triangle-seq time-left))))

(defn should-cutoff [blueprint state best-so-far]
  (let [{:keys [time-left resources robots]} state]
    (or
     (= time-left 0)
     (< (get resources :geode 0) (best-so-far time-left))
     (<= (max-geode-potential state) (get best-so-far 0 -1)))))


rich-state
(next-states-better blueprint rich-state)

(defn get-next-best-so-far [{:keys [time-left resources]} best-so-far]
  (assoc best-so-far time-left (max (get best-so-far time-left -1) (get resources :geode 0))))

(defn best-score-dfs [blueprint state best-so-far visited nodes]
  ;; for this to work, we need to return a best-so-far visited nodes.
  ;; e.g. this is monadic.
  (let [best-so-far (get-next-best-so-far state best-so-far)]
    (cond
      (should-cutoff blueprint state best-so-far) [best-so-far visited nodes]
      :else
      (reduce
       (fn [[best-so-far visited nodes] neighbor]
         ;; TODO: we can exploit neighbor ordering + cutoffs to skip all
         ;; neighbors in the event of a cutoff.
         (cond
           (contains? visited neighbor) [best-so-far visited nodes]
           :else
           (best-score-dfs blueprint neighbor best-so-far (conj visited neighbor) (inc nodes))))
       [best-so-far visited nodes]
       (next-states-better blueprint state)))))

;; we're going to use DFS here.
(defn best-score-part1 [blueprint]
  (best-score-dfs
   blueprint
   {:time-left 24
    :resources {:ore 0 :geode 0 :obsidian 0 :clay 0}
    :robots {:ore 1}}
   {} #{} 0))

(defn best-score-part2 [blueprint]
  (best-score-dfs
   blueprint
   {:time-left 32
    :resources {:ore 0 :geode 0 :obsidian 0 :clay 0}
    :robots {:ore 1}}
   {} #{} 0))

(best-score-part2 blueprint)

(def blueprint2 "Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.")

(defn search-part1 [blueprint]
  (let [[best-scores _ nodes] (best-score-part1 blueprint)
        score (best-scores 0)]
    (println "blueprint" (blueprint :number) "has max" score "with" nodes "nodes")
    score))

(defn search-part2 [blueprint]
  (let [[best-scores _ nodes] (best-score-part2 blueprint)
         score (best-scores 0)]
     (println "blueprint" (blueprint :number) "has max" score "with" nodes "nodes (part 2)")
     score))

(defn total-quality [blueprint-list]
  (reduce
   +
   (map
    (fn [blueprint] (* (blueprint :number) (search-part1 blueprint)))
    blueprint-list)))

(println
 "total quality (examples)"
 (total-quality [blueprint (parse-blueprint blueprint2)]))

(println
 "total quality (input)"
 (total-quality (map parse-blueprint (utils/read-resource-lines "input/day19.txt"))))

(def input-blueprints
  (map parse-blueprint (utils/read-resource-lines "input/day19.txt")))


(println "answer to part 2" (reduce * (map search-part2 (take 3 input-blueprints))))
