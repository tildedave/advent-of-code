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
              acc))
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

(defn complete-workers [state]
  (let [{:keys [workers]} state
        just-done-workers (->> workers
                               (map (fn [[worker-num {:keys [letter time-left]}]]
                                      (if (and (zero? time-left)
                                               (not (nil? letter)))
                                        [worker-num letter]
                                        nil)))
                               (remove nil?))
        just-done-letters (set (map second just-done-workers))
        next-workers (reduce
                      (fn [workers worker-num]
                        (assoc-in workers [worker-num :letter] nil))
                      workers
                      (map first just-done-workers))]
    (-> state
        (update :keys-left #(set/difference % just-done-letters))
        (update :deps #(update-vals % (fn [s] (set/difference s just-done-letters))))
        (update :order #(into % just-done-letters))
        (assoc :workers next-workers))))

(defn assign-new-workers [state tax]
  (let [{:keys [keys-left workers deps]} state
        idle-workers (->> workers
                          (filter (fn [[_ {:keys [time-left]}]] (zero? time-left)))
                          (map first))
        still-working-letters (set (remove nil? (map :letter (vals workers))))
        available-keys (ready-keys deps (set/difference keys-left still-working-letters))
        new-assignments (->> (map vector idle-workers available-keys)
                             (into {}))]
    (update state :workers (fn [workers] (->> workers
                                              (map (fn [[worker-num {:keys [time-left letter]}]]
                                                     [worker-num
                                                      (if-let [letter (new-assignments worker-num)]
                                                        {:time-left (+ tax (- (int (.charAt letter 0)) 65))
                                                         :letter letter}
                                                        {:time-left (max (dec time-left) 0) :letter letter})]))
                                              (into {}))))))

(defn topo-sort-with-workers [tax num-workers backward-deps]
  (let [all-keys (set/union (set (keys backward-deps))
                            (reduce set/union (vals backward-deps)))
        starting-workers (->> (range num-workers)
                              (map #(hash-map % {:time-left 0 :letter nil}))
                              (reduce into {}))]
    (->> (iterate
         ;; it's easiest for iterate to "tick" a second forward,
         ;; even though this is a lot of extra states.
          (fn [[curr-second state]]
            [(inc curr-second)
             (-> state
                 (complete-workers)
                 (assign-new-workers tax))])
          [0 {:keys-left all-keys
              :order []
              :workers starting-workers
              :deps backward-deps}])
    ;; )))
         (reduce
          (fn [acc [curr-second {:keys [keys-left]}]]
            (if (empty? keys-left)
              (reduced (dec curr-second))
              acc))))))

(->> (utils/read-input "2018/day7.txt")
     (map parse-step)
     (reduce (partial merge-with set/union) {})
     (backward-deps)
     (topo-sort-with-workers 60 5)
    ;;  (take 3)
    ;;  (last)
    ;;  (println)
    ;;  (take 16)
    ;;  (last)
     )
