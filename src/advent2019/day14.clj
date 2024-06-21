(ns advent2019.day14
  (:require [utils :as utils]
            [graph :as graph]))

;; awful but whatever.
(defn parse-line [^String line]
  (let [[left right] (.split line " => ")
        [production-amount production-element] (.split right " ")]
    {production-element {:amount (utils/parse-int production-amount)
                         :dependencies (into {} (map #(vec (reverse (map utils/try-parse-int (.split % " ")))) (.split left ", ")))}}))

;; so I guess I did this before. (day14.go)
;; topo sort chemicals in order (does this matter?  yes it seems to)
;; always take from an earlier chemical if you can.

(defn read-production-rules [filename]
  (->> (utils/read-input filename)
       (map parse-line)
       (reduce merge {})))

(defn to-graph [production-rules]
  (->> production-rules
       (map (fn [[result {:keys [dependencies]}]]
              {result (set (keys dependencies))}))
       (reduce merge {})))

(def ^:dynamic eat-surplus-immediately? false)

;; I suppose A* might work for this, actually.
;; production-rules determine the transitions.

;; this is a translation of my golang algorithm
(defn minimum-ore [production-rules fuel-amount]
  (let [graph (to-graph production-rules)
        order (graph/topological-sort graph)]
    (loop [needed {"FUEL" fuel-amount}
           surplus {}]
      (let [chemical (->> order (filter needed) (remove (partial = "ORE")) (first))
            amount-needed (needed chemical)]
        (cond
          (nil? chemical) (needed "ORE")
          (and (contains? surplus chemical) eat-surplus-immediately?)
        ;; branch 1: immediately eat surplus
        ;; I think this is wrong since you might need to get some more surplus
        ;; via some other transformation rules.
          (recur
           (update needed chemical #(- % (surplus chemical)))
           (dissoc surplus chemical))
          (>= (get surplus chemical 0) (needed chemical))
        ;; branch 2: eat surplus only if you have more than the needed amount
        ;; (my previous golang solution)
          (recur
           (update needed chemical #(- % (surplus chemical)))
           (dissoc surplus chemical))
        ;; branch 3: apply a production rule
          :else
          (let [amount-produced (:amount (production-rules chemical))
                has-surplus? (not= 0 (mod amount-needed amount-produced))
                application-times (+
                                   (quot amount-needed amount-produced)
                                   (if has-surplus? 1 0))
                surplus-amount (- (* application-times amount-produced) amount-needed)]
            (recur
             (reduce
              (fn [needed [chemical amount]]
                (update needed chemical (fnil (partial + amount) 0)))
              (dissoc needed chemical)
              (map (fn [[k v]] [k (* v application-times)]) (:dependencies (production-rules chemical))))
             (update surplus chemical (fnil (partial + surplus-amount) 0)))))))))

(binding [eat-surplus-immediately? true]
  (assert (= 31 (minimum-ore (read-production-rules "2019/day14-example.txt")
                             1)))
  (assert (= 165
             (minimum-ore (read-production-rules "2019/day14-example2.txt") 1)))
  (assert (= 13312
             (minimum-ore (read-production-rules "2019/day14-example3.txt") 1)))
  (assert (= 180697
             (minimum-ore (read-production-rules "2019/day14-example4.txt") 1)))
  (assert (= 2210736
             (minimum-ore (read-production-rules "2019/day14-example5.txt") 1))))

;; interestingly, eat-surplus-immediately is correct

;; part 1: this is correct
(minimum-ore (read-production-rules "2019/day14.txt") 1)

;; for part 2 I did a binary search
;; there are apparently faster ways to do it (reddit thread),
;; but we'll just do something similar to our golang approach.
;; we want to find the max fuel amount so that the amount of ore to get it
;; is < 1 trillion

(defn part2 [filename]
  (let [production-rules (read-production-rules filename)
        ore-per-one-fuel (minimum-ore production-rules 1)
        target-ore 1000000000000]
    (loop [guess-low (quot target-ore ore-per-one-fuel)
           guess-high (* 2 (quot target-ore ore-per-one-fuel))]
      (cond
        (= guess-low guess-high) guess-low
        (= (inc guess-low) guess-high) guess-low
        :else
        (let [current-guess (quot (+ guess-low guess-high) 2)
              ore-for-guess (minimum-ore production-rules current-guess)]
          (case (compare target-ore ore-for-guess)
            -1 (recur guess-low current-guess)
            0 current-guess
            1 (recur current-guess guess-high)))))))

(println (part2 "2019/day14-example3.txt"))
(println (part2 "2019/day14-example4.txt"))
(println (part2 "2019/day14-example5.txt"))
(println (part2 "2019/day14.txt"))
