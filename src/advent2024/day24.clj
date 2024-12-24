(ns advent2024.day24
  (:require
    [utils :as utils]))

(defn parse-signal [^String line]
  (let [[signal pulse] (.split  #": " line)]
    {signal (utils/parse-int pulse)}))

(defn parse-connector [^String line]
  (let [[_ left conn right output] (re-matches #"^(\w+) (AND|OR|XOR) (\w+) -> (\w+)$" line)]
    [left right (keyword (.toLowerCase conn)) output]))

;; the number of connectors is low now so we will do something silly

(defn run-signals [connectors signal-values]
  (reduce
   (fn [signals [left right operation output]]
     (if (and (contains? signals left) (contains? signals right))
       (assoc signals output ((case operation
                               :and bit-and
                               :xor bit-xor
                               :or bit-or)  (signals left) (signals right)))
       signals))
   signal-values
   connectors))

(defn parse-system [lines]
  (let [[signals connectors] (utils/split-by "" lines)]
    [(reduce merge (map parse-signal signals))
     (map parse-connector connectors)]))

;; (run-system (utils/read-input "2024/day24.txt"))

(def every-z (memoize (fn [connectors]
  (->> (map #(nth % 3) connectors)
       (filter #(.startsWith % "z"))))))

(set (every-z (second (parse-system (utils/read-input "2024/day24.txt")))))

(defn has-every-z? [z-set signals]
  (every? #(contains? signals %) z-set))

(defn z-values [z-set signals]
  (->> (sort (comp - compare) z-set)
       (map signals)
       (reduce
        (fn [acc z-val]
          (println acc z-val)
          (bit-or (bit-shift-left acc 1) z-val))
        0)))

(defn run-system [lines]
  (let [[initial-signals connectors] (parse-system lines)
        z-set (every-z connectors)]
    (->> (iterate (partial run-signals connectors) initial-signals)
         (drop-while #(not (has-every-z? z-set %)))
         (first)
         (z-values z-set))))

(run-system (utils/read-input "2024/day24-example.txt"))
