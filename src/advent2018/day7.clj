(ns advent2018.day7
  (:require [utils :as utils]
            [clojure.set :as set]
            [clojure.string :as string]))

(def step-re #"Step ([A-Z]) must be finished before step ([A-Z]) can begin.")

(defn parse-step [s]
  (let [[s1 s2] (rest (re-matches step-re s))]
    {s1 #{s2}}))

(defn backward-deps [deps]
  (reduce
   (partial merge-with set/union)
   {}
   (for [[k v] deps
         v' v]
     {v' #{k}})))

(defn topo-sort [backward-deps]
  (let [all-keys (set/union (set (keys backward-deps))
                            (reduce set/union (vals backward-deps)))]
    (->> (iterate
          (fn [[order keys-left deps]]
            (let [next-dep (->> keys-left
                                (filter #(empty? (get deps % #{})))
                                (sort)
                                (first))]
              [(conj order next-dep)
               (disj keys-left next-dep)
               (update-vals deps #(disj % next-dep))]))
          [[] all-keys backward-deps])
         (reduce
          (fn [acc [order keys-left deps]]
            (if (empty? keys-left)
              (reduced (string/join order))
              acc
            ))
          nil))))

(->> (utils/read-input "2018/day7.txt")
     (map parse-step)
     (reduce (partial merge-with set/union) {})
     (backward-deps)
     (topo-sort))
