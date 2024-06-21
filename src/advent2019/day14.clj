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

(defn read-production-rules [lines]
  (->> lines
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

(read-production-rules "2019/day14-example.txt")

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

(minimum-ore (read-production-rules (utils/read-input "2019/day14-example.txt"))
             1)

(assert (= 165
        (minimum-ore
         (read-production-rules
          '("9 ORE => 2 A"
            "8 ORE => 3 B"
            "7 ORE => 5 C"
            "3 A, 4 B => 1 AB"
            "5 B, 7 C => 1 BC"
            "4 C, 1 A => 1 CA"
            "2 AB, 3 BC, 4 CA => 1 FUEL")) 1)))

(assert (= 13312
           (minimum-ore
            (read-production-rules
             '("157 ORE => 5 NZVS"
               "165 ORE => 6 DCFZ"
               "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
               "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
               "179 ORE => 7 PSHF"
               "177 ORE => 5 HKGWZ"
               "7 DCFZ, 7 PSHF => 2 XJWVT"
               "165 ORE => 2 GPVTF"
               "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"))
                        1)))

;; this is correct
(minimum-ore (read-production-rules (utils/read-input "2019/day14.txt")) 1)
