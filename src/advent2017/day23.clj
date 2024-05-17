(ns advent2017.day23
  (:require [utils :as utils]))

(defn parse-instr [line]
  (if-let [[_ op arg1 _ arg2] (re-matches #"^(set|sub|mul|jnz) (-?\w+)( (-?\w+))?$" line)]
    [(keyword op) (utils/try-parse-int arg1) (utils/try-parse-int arg2)]
    (throw (Exception. (format "Could not parse %s" line)))))

(defn step [program]
  (fn [state]
    (let [{:keys [registers pc]} state]
      (if-let [[instr x y] (get program pc)]
        (let [resolve (fn [x]
                        (cond (nil? x) nil
                              (number? x) x
                              :else (get registers x)))]
          (case instr
            :set (-> state
                     (assoc-in [:registers x] (resolve y))
                     (update :pc inc))
            :sub (-> state
                     (assoc-in [:registers x] (- (resolve x) (resolve y)))
                     (update :pc inc))
            :mul (-> state
                     (assoc-in [:registers x] (* (resolve x) (resolve y)))
                     (update :pc inc))
            :jnz (if (not= (resolve x) 0)
                   (update state :pc (partial + (resolve y)))
                   (update state :pc inc))))
        state))))

(defn initial-state []
  {:pc 0
   :registers {"a" 0 "b" 0 "c" 0 "d" 0 "e" 0 "f" 0 "g" 0 "h" 0}})

(defn answer-part1 []
  (let [program (->> (utils/read-input "2017/day23.txt")
                     (mapv parse-instr))]
    (->> (iterate (step program) (initial-state))
         (partition-by #(contains? program (:pc %)))
         (first)
         (map #(first (get program (:pc %))))
         (filter (partial = :mul))
         (count))))


(let [program (->> (utils/read-input "2017/day23-modded.txt")
                   (mapv parse-instr))]
  (->> (iterate (step program) (initial-state))
       (partition-by #(contains? program (:pc %)))
       (first)
       (last)))


(let [program (->> (utils/read-input "2017/day23.txt")
                   (mapv parse-instr))]
  (iterate (step program) (assoc-in (initial-state) [:registers "a"] 1)))

(- 123700 106700)
