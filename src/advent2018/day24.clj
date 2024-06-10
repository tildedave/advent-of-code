(ns advent2018.day24
  (:require [utils :as utils]
            [clojure.set :as set]))

(defn parse-unit [n s]
  (let [extract-number (fn [re] (utils/parse-int (second (re-find re s))))
        initiative (extract-number #"at initiative (\d+)")
        num-units (extract-number #"(\d+) units each")
        hp (extract-number #"(\d+) hit points")
        attack-power (extract-number #"with an attack that does (\d+)")
        attack-type (second (re-find #"attack that does \d+ (\w+) damage" s))
        ^String weakness-str (second (re-find #"weak to ([^);]*)" s))
        ^String immunity-str (second (re-find #"immune to ([^);]*)" s))]
    {:id (random-uuid)
     :initiative initiative
     :num-units num-units
     :hp hp
     :group (inc n)
     :attack-power attack-power
     :attack-type (keyword attack-type)
     :weakness (if weakness-str (set (map keyword (.split weakness-str ", "))) nil)
     :immunity (if immunity-str (set (map keyword (.split immunity-str ", "))) nil)}))

(parse-unit 0 "216 units each with 7514 hit points (immune to cold, slashing; weak to bludgeoning) with an attack that does 335 slashing damage at initiative 15")

(defn parse-armies [filename]
  (->> (utils/read-input filename)
       (utils/split-by "")
       (map rest)
       (map #(map-indexed parse-unit %))
       (map-indexed (fn [n l] (map #(assoc % :type (case n 0 :immune :infection)) l)))
       (reduce concat)))

(parse-armies "2018/day24-example.txt")

(defn effective-power [army]
  (* (:num-units army) (:attack-power army)))

(effective-power (parse-unit "18 units each with 729 hit points (weak to fire; immune to cold, slashing) with an attack that does 8 radiation damage at initiative 10"))

(defn target-selection-order-compare [army1 army2]
  (let [ep1 (effective-power army1)
        ep2 (effective-power army2)]
    (case (compare ep2 ep1)
      -1 -1
      1 1
      0 (compare (:initiative army2) (:initiative :army1)))))

(defn expected-damage [source-army target-army]
  (let [ep (effective-power source-army)]
    (cond
      (contains? (:immunity target-army) (:attack-type source-army)) 0
      (contains? (:weakness target-army) (:attack-type source-army)) (* 2 ep)
      :else ep)))

(defn target-selection-target-compare [source-army army1 army2]
  (let [ed1 (expected-damage source-army army1)
        ed2 (expected-damage source-army army2)
        ep1 (effective-power army1)
        ep2 (effective-power army2)]
    (case (compare ed2 ed1)
      -1 -1
      1 1
      (case (compare ep2 ep1)
        -1 1
        1 1
        (compare (:initiative army2) (:initiative army1))))))

(defn short-army [{:keys [group type]}]
  {:group group :type type})

;; result is a map from uuid to uuid
(defn target-selection [armies]
  (set/map-invert
   (reduce
    (fn [targeted-by army]
      (let [target (->> armies
                        (remove #(= (:type %) (:type army)))
                        (remove #(contains? targeted-by (:id %)))
                        (remove #(zero? (expected-damage army %)))
                        (sort (partial target-selection-target-compare army))
                        (first))]
        (if (nil? target)
          targeted-by
        ;;   (do (println (short-army army) "targets" (short-army target) "expecting" (expected-damage army target))
          (assoc targeted-by (:id target) (:id army)))))
    {}
    (sort target-selection-order-compare armies))))

;; (target-selection (parse-armies "2018/day24-example.txt"))

(defn process-attacks [armies targets]
  (vals
   (reduce
    (fn [armies-by-id army-id]
          ;; need to refetch as we might have taken damage during this round
      (if-let [target-id (targets army-id)]
        (let [army (armies-by-id army-id)
              target-army (armies-by-id target-id)
              damage (expected-damage army target-army)
              dead-units (quot damage (:hp target-army))
              _ (println (short-army army) "attacks" (short-army target-army) "damage" damage "killing" dead-units)]
          (update-in armies-by-id [target-id :num-units]
                     (fn [curr-units]
                       (max (- curr-units dead-units) 0))))
        armies-by-id))
    (reduce merge {} (map #(hash-map (:id %) %) armies))
    (map :id (sort-by :initiative > armies)))))

(defn process-round [armies]
  (process-attacks armies (target-selection armies)))

(defn is-over? [armies]
  (let [by-type (group-by :type armies)]
    (or (every? #(zero? (:num-units %)) (by-type :immune))
        (every? #(zero? (:num-units %)) (by-type :infection)))))

;; this is our answer, but we are off by a few points somewhere.
(->> (parse-armies "2018/day24-example.txt")
     (iterate process-round)
     (drop-while #(not (is-over? %)))
     (first)
     (map :num-units)
     (reduce +))
