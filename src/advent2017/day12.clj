(ns advent2017.day12
  (:require [utils :as utils]
            [clojure.set :as set]))

(defn parse-line [^String s]
  (if-let [[_ source destlist] (re-matches #"^(\d+) <-> (.*)$" s)]
    {:source (utils/parse-int source)
     :dests (set (map utils/parse-int (.split #",\s" destlist)))}
    (throw (Exception. (format "Could not parse %s" s)))))

(parse-line "2 <-> 0, 3, 4")

(defn merge-forests [forests {:keys [source dests]}]
  (let [s (conj dests source)
        [next-forests next]
        (reduce
         (fn [[next-forests so-far] forest]
           (let [overlap (set/intersection so-far forest)]
             (cond
               (empty? overlap) [(conj next-forests forest) so-far]
               :else [next-forests (set/union so-far forest)])))
         [[] s]
         forests)]
    (conj next-forests next)))

(defn answer [forests]
  (reduce
   (fn [acc forest]
     (if (contains? forest 0)
       (reduced (count forest))
       nil))
   nil
   forests))

;; part 1

(->> (utils/read-input "2017/day12-example.txt")
     (map parse-line)
     (reduce merge-forests [])
     (answer))

(->> (utils/read-input "2017/day12.txt")
     (map parse-line)
     (reduce merge-forests [])
     (answer))

;; part 2

(->> (utils/read-input "2017/day12.txt")
     (map parse-line)
     (reduce merge-forests [])
     (count))
