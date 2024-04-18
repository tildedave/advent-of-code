(ns advent2015.day21
  (:require [clojure.math.combinatorics :as combo]
            [utils :as utils]))


(defn fight [p1 p2
             p1-act?]
  (let [{p1-hp :hit-points
         p1-armor :armor
         p1-damage :damage} p1
        {p2-hp :hit-points
         p2-armor :armor
         p2-damage :damage} p2]
    (cond
      (<= p1-hp 0) :p2-wins
      (<= p2-hp 0) :p1-wins
      p1-act?
      (recur
       p1
       (-> p2
           (update :hit-points #(- % (max (- p1-damage p2-armor) 1))))
       (not p1-act?))
      :else
      (recur
       (-> p1 (update :hit-points #(- % (max (- p2-damage p1-armor) 1))))
       p2
       (not p1-act?)))))

(fight {:hit-points 8 :damage 5 :armor 5}
       {:hit-points 12 :damage 7 :armor 2}
       true)

(def weapons
  {:dagger {:cost 8 :damage 4 :armor 0}
   :shortsword {:cost 10 :damage 5 :armor 0}
   :warhammer {:cost 25 :damage 6 :armor 0}
   :longsword {:cost 40 :damage 7 :armor 0}
   :greataxe {:cost 74 :damage 8 :armor 0}})

(def armor
  {:leather {:cost 13 :damage 0 :armor 1}
   :chainmail {:cost 31 :damage 0 :armor 2}
   :splintmail {:cost 53 :damage 0 :armor 3}
   :bandedmail {:cost 75 :damage 0 :armor 4}
   :platemail {:cost 102 :damage 0 :armor 5}})

(def rings
  {:damage-plus-1 {:cost 25 :damage 1 :armor 0}
   :damage-plus-2 {:cost 50 :damage 2 :armor 0}
   :damage-plus-3 {:cost 100 :damage 3 :armor 0}
   :defense-plus-1 {:cost 20 :damage 0 :armor 1}
   :defense-plus-2 {:cost 40 :damage 0 :armor 2}
   :defense-plus-3 {:cost 80 :damage 0 :armor 3}})

(defn min-gold-for [kw target]
  (cond
    (and (= kw :armor) (= target 0)) 0
    :else (->> (for [[_ thing] (case kw :damage weapons :armor armor)]
                 (->> (range 3)
                      (map (fn [n]
                             (map #(conj % thing)
                                  (combo/selections (vals rings) n))))
                      (apply concat)))
               (apply concat)
               (filter #(>= (reduce + (map kw %)) target))
               (map #(reduce + (map :cost %)))
               (sort)
               (first))))

(defn max-gold-for [kw target]
  (cond
    (and (= kw :armor) (= target 0)) 0
    :else (->> (for [[_ thing] (case kw :damage weapons :armor armor)]
                 (->> (range 3)
                      (map (fn [n]
                             (map #(conj % thing)
                                  (combo/selections (remove #(zero? (kw %)) (vals rings)) n))))
                      (apply concat)))
               (apply concat)
               (concat (case kw
                         :damage []
                         :armor (->>
                                 (range 3)
                                 (map (fn [n] (combo/selections (remove #(zero? (kw %)) (vals rings)) n)))
                                 (apply concat))))
               (filter #(= (reduce + (map kw %)) target))
            ;;    (sort-by #(reduce + (map :cost %)) >))))
               (map #(reduce + (map :cost %)))
               (sort >)
               (first))))

(max-gold-for :damage 4)
(min-gold-for :damage 14)

;; so we just probe which values beat the boss and of the "critical points"
;; we find the minimum gold for it.
;; if you beat the boss with armor x and damage y you beat it with x+1 and y+1 too.
;; we will end up with a list of "critical points" where you win with x/y but
;; lose with x - 1 or y - 1.

(defn parse-boss [filename]
  (->> (utils/read-input filename)
       (map #(.split % ": "))
       (map (fn [[x num]] {(case x
                             "Hit Points" :hit-points
                             "Damage" :damage
                             "Armor" :armor)
                           (utils/parse-int num)}))
       (apply merge)))

(parse-boss "2015/day21.txt")

(fight {:hit-points 100 :damage 0 :armor 1}
       (parse-boss "2015/day21.txt")
       true)


;; this would definitely be easier in golang or something to do the loop I want.
;; instead we'll do everything.

(map #(min-gold-for :damage %) (range))

(last (first (partition-by #(nil? (min-gold-for :damage %)) (range))))

(defn all-damage-armor-pairs []
  (let [max-damage (->> (range)
                        (partition-by #(nil? (min-gold-for :damage %)))
                        (first)
                        (last))
        max-armor (->> (range)
                       (partition-by #(nil? (min-gold-for :armor %)))
                       (first)
                       (last))]
    (->>
     (for [d (range (inc max-damage))
           a (range (inc max-armor))]
       [d a]))))

(defn answer-p1 [filename]
  (let [player {:hit-points 100 :damage 0 :armor 0}
        boss (parse-boss filename)]
    (->> (all-damage-armor-pairs)
     (filter (fn [[d a]]
               (=
                :p1-wins
                (fight (-> player
                           (assoc :damage d)
                           (assoc :armor a))
                       boss
                       true))))
     (map (fn [[d a]] (+ (min-gold-for :damage d)
                         (min-gold-for :armor a))))
     (sort)
     (first))))


(answer-p1 "2015/day21.txt")

(defn answer-p2 [filename]
  (let [player {:hit-points 100 :damage 0 :armor 0}
        boss (parse-boss filename)]
    (->> (all-damage-armor-pairs)
         (filter (fn [[d a]]
                   (=
                    :p2-wins
                    (fight (-> player
                               (assoc :damage d)
                               (assoc :armor a))
                           boss
                           true))))
         (filter (fn [[d a]] (and (number? (max-gold-for :damage d)) (number? (max-gold-for :armor a)))))
         (map (fn [[d a]] (+ (max-gold-for :damage d)
                             (max-gold-for :armor a))))
         (sort >)
         (first))))

(max-gold-for :damage 4)

(answer-p2 "2015/day21.txt")
