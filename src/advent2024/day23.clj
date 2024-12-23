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

;; slow
;; (->> (reduce graph-reducer {} (utils/read-input "2024/day23.txt"))
;;      three-sets
;;      (filter has-t?)
;;      (count))

(defn graph-visualization [components]
  (string/join "\n\t"
  (for [[k vs] components]
    (string/join "\n\t"
     (map #(format "%s -- %s;" k %) vs)))))

;; (spit "graph23.dot"
;;      (str
;;  "graph G {\n\t"
;;  (graph-visualization (reduce graph-reducer {} (utils/read-input "2024/day23.txt")))
;;  "\n}"))

(defn maximal-cliques [graph]
  (loop
   [queue [[#{} (set (keys graph)) #{}]]
    all-cliques []]
    (if-let [[R P X] (first queue)]
      (if (and (empty? P) (empty? X))
        (recur (rest queue) (conj all-cliques R))
        (let [[queue _ _]
              (reduce
               (fn [[queue P X] v]
                 [(conj queue [(conj R v) (set/intersection P (graph v)) (set/intersection X (graph v))])
                  (disj P v)
                  (conj P v)])
               [(rest queue) P X]
               P)]
          (recur queue all-cliques)))
      all-cliques)))

(->> (utils/read-input "2024/day23.txt")
     (reduce graph-reducer {})
     (maximal-cliques)
     (sort-by count >)
     (first)
     (sort)
     (string/join ","))
