(ns advent2024.day23
  (:require
   [utils :as utils]
   [clojure.string :as string]
   [clojure.set :as set]))

(.split #"\-" "kh-tc")

(defn graph-reducer [components line]
  (let [[left right] (.split #"\-" line)]
    (-> components
        (update left (fnil conj #{}) right)
        (update right (fnil conj #{}) left))))

(reduce graph-reducer {} (utils/read-input "2024/day23-example.txt"))

(defn three-sets [components]
  (distinct
   (for [k1 (keys components) k2 (keys components) k3 (keys components)
        :when (and (not= k1 k2) (not= k2 k3) (not= k1 k3)
                   ((components k1) k2)
                   ((components k1) k3)
                   ((components k2) k3))]
    #{k1 k2 k3})))

(defn has-t? [set]
  (seq (filter #(.startsWith % "t") set)))

(has-t? #{"co" "de" "ka"})

(defn maximal-cliques [graph]
  (letfn
   [(bron-kerbosch [R P X]
      (if (and (empty? P) (empty? X))
        #{R}
        (let [pivot (first (set/union P X))
              [all-cliques _ _]
              (reduce
               (fn [[all-cliques P X] v]
                 [(set/union
                   all-cliques
                   (bron-kerbosch
                    (conj R v)
                    (set/intersection P (graph v))
                    (set/intersection X (graph v))))
                  (disj P v)
                  (conj P v)])
               [#{} P X]
               (set/difference P (graph pivot)))]
          all-cliques)))]
    (bron-kerbosch #{} (set (keys graph)) #{})))

(->> (utils/read-input "2024/day23.txt")
     (reduce graph-reducer {})
     (maximal-cliques)
     (sort-by count >)
     (first)
     (sort)
     (string/join ",")
     (time))
