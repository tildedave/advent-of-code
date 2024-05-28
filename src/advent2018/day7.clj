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

(defn ready-keys [deps keys-left]
  (->> keys-left
       (filter #(empty? (get deps % #{})))
       (sort)))

(interleave '(1 2 3  7 8 9) '(4 5 6))
(into {} [[1 2] [3 4]])
(int \A)


(defn topo-sort-with-workers [tax num-workers backward-deps]
  (let [all-keys (set/union (set (keys backward-deps))
                            (reduce set/union (vals backward-deps)))
        starting-workers (->> (range num-workers)
                              (map #(hash-map % {:time-left 0 :letter nil}))
                              (reduce into {}))]
    (->> (iterate
         ;; it's easiest for iterate to "tick" a second forward,
         ;; even though this is a lot of extra states.
          (fn [[curr-second order keys-left workers deps]]
            (let [just-done-workers (->> workers
                                         (map (fn [[worker-num {:keys [letter time-left]}]]
                                                (if (and (zero? time-left)
                                                         (not (nil? letter)))
                                                  [worker-num letter]
                                                  nil)))
                                         (remove nil?))
                  just-done-letters (set (map second just-done-workers))
                  workers (reduce
                           (fn [workers worker-num]
                             (assoc-in workers [worker-num :letter] nil))
                           workers
                           (map first just-done-workers))
                  ;; _ (println "just-done-letters" just-done-letters)
                  order (into order just-done-letters)
                  idle-workers (->> workers
                                    (filter (fn [[_ {:keys [time-left]}]] (zero? time-left)))
                                    (map first))
                  ;; _ (println "idle-workers" idle-workers)
                  working-letters (set (remove nil? (map :letter (vals workers))))
                  keys-left (set/difference keys-left just-done-letters)
                  deps (update-vals deps #(set/difference % just-done-letters))
                  new-assignments (->> (map vector idle-workers (ready-keys deps (set/difference keys-left working-letters)))
                                       (into {}))
                  ;; _ (println "new assigments" new-assignments)
                  next-workers (->> workers
                                    (map (fn [[worker-num {:keys [time-left letter]}]]
                                           [worker-num
                                            (if-let [letter (new-assignments worker-num)]
                                              {:time-left (+ tax (- (int (.charAt letter 0)) 65))
                                               :letter letter}
                                              {:time-left (max (dec time-left) 0) :letter letter})]))
                                    (into {}))]
              [(inc curr-second)
               order
               keys-left
               next-workers
               deps]))
          [0 [] all-keys starting-workers backward-deps])
         (reduce
          (fn [acc [curr-second _ keys-left]]
            (if (empty? keys-left)
              (reduced (dec curr-second))
              acc)))
  )))

(->> (utils/read-input "2018/day7.txt")
     (map parse-step)
     (reduce (partial merge-with set/union) {})
     (backward-deps)
     (topo-sort-with-workers 60 5)
    ;;  (println)
    ;;  (take 16)
    ;;  (last)
     )
