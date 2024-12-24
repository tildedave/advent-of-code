(ns advent2024.day24
  (:require
   [utils :as utils]
   [clojure.math :as math]))

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
          (bit-or (bit-shift-left acc 1) z-val))
        0)))

(defn number-to-signals [num-bits number prefix]
  ;; our numbers are 45 bits so that's all we need
  (->> (range 0 (inc num-bits))
       (map (fn [n] (hash-map (format "%s%02d" prefix n) (if (bit-test number n) 1 0))))
       (reduce merge)))

(def computation-limit 50)
(number-to-signals 4 11 "x")

(defn run-system [lines]
  (let [[initial-signals connectors] (parse-system lines)
        z-set (every-z connectors)]
    (if-let [result (->> (iterate (partial run-signals connectors) initial-signals)
                         (take computation-limit)
                         (drop-while #(not (has-every-z? z-set %)))
                         (first))]
      (z-values z-set result)
      nil)))

(assert (= (run-system (utils/read-input "2024/day24.txt")) 56939028423824))

;; OK for part 2 we have a malfunctioning adder
;; this is a pretty cool kind of problem
;; so we need a way to figure out what kinds of numbers break
;; test a bunch of numbers
;; figure out which bits are wrong

(defn test-system [connectors x y]
  (let [z-set (every-z connectors)]
    (if-let [result (->> (iterate (partial run-signals connectors) (merge (number-to-signals 45 x "x") (number-to-signals 45 y "y")))
         (take computation-limit)
         (drop-while #(not (has-every-z? z-set %)))
         (first))]
      (z-values z-set result)
      nil)))

(defn incorrect-bits [connectors x y]
  (let [result (test-system connectors x y)
        correct-result (+ x y)]
    (cond
      (nil? result) (list :no-z-output)
      (not= result correct-result)
      (let [differences (bit-xor result correct-result)]
        (->> (range 0 45)
             (filter #(bit-test differences %))))
      :else '())))

;; we'll want this because we want to swap outputs
(def system (utils/read-input "2024/day24.txt"))
(def parsed-connectors (second (parse-system (utils/read-input "2024/day24.txt"))))

(test-system parsed-connectors (bit-or 1 (bit-shift-left 0xFFFFFFFFFFF 1)) 1)

;; first issue: bit 5 seems to be broken
;; then something else is broken around bit 16 (otherwise the issue would keep cascading)
;; then something else is broken around bit 39
;; one of these must have multiple breakages
(incorrect-bits parsed-connectors 0xFFFFFFFFFFF 1)
(math/log (bit-or 1 (bit-shift-left 0xFFFFFFFFFFF 1)))
(incorrect-bits parsed-connectors (bit-or 1 (bit-shift-left 0xFFFFFFFFFFF 1)) 1)
(incorrect-bits parsed-connectors 0x0000000001F 1)

(defn log2 [n] (/ (math/log n) (math/log 2)))

;; this one seems to be wrong
(incorrect-bits parsed-connectors 31 1)

;; OK, we can do this manually (look at the connectors)
;; we can find for each bit, what outputs affect it
;; however: connectors are good
;; swap all pairs of outputs that are suspect
;; this works until we have a gate with multiple breakages
;; OK so what we'd like to do is point a bit is bad.
;; then it goes BACKWARDS until it finds a known-good output.
;; however, what's a known good output?

;; srp OR jcf -> z05  ---> clearly wrong, needs to be swapped with something else
;; x21 AND y21 -> z21 ---> clearly wrong, needs to go to the carry bit
;; > Your system of gates and wires has four pairs of gates which need their output wires swapped
;; FOUR pairs of gates.  so we have found two of the outputs, six remain.

;; attempt to understand the output enough:
(defn expression [connectors output max-depth]
  (letfn [(rec [output depth]
               (if (zero? depth)
                 output
                 (if-let [[left right connector _]
                          (->> connectors
                               (filter #(= (nth % 3) output))
                               (first))]
                   [(rec left (dec depth))
                    connector
                    (rec right (dec depth))]
                   output)))]
    (rec output max-depth)))

(expression parsed-connectors "z03" 2)

(for [x (range 0 17)
      y (range 0 17)
      :let [bad-bits (incorrect-bits parsed-connectors x y)]
      :when (seq bad-bits)]
  [x y bad-bits])


(defn test-full-system [connectors x y]
  (let [z-set (every-z connectors)]
    (->> (iterate (partial run-signals connectors) (merge (number-to-signals 45 x "x") (number-to-signals 45 y "y")))
         ;; just bound our computation
         (take computation-limit)
         (drop-while #(not (has-every-z? z-set %)))
         (first))))

;; maybe we can find where this 1 goes
(filter
 (fn [[k v]] (and (= v 1) (nil? (re-find #"^x|y|z" k))))
 (test-full-system parsed-connectors 16 16))

;; so the output is one of
;; (["qjq" 1] ["z06" 1] ["frn" 1] ["ccq" 1])
;;
(defn swap-outputs [connectors out1 out2]
  (map (fn [[_ _ _ out :as row]]
         (cond
           (= out out1) (assoc row 3 out2)
           (= out out2) (assoc row 3 out1)
           :else row)) connectors))

(time
 (for [candidate '("frn" "ccq" "qjq")]
  [candidate (incorrect-bits (swap-outputs parsed-connectors "z05" candidate) 31 1)]))

;; OK so frn <-> z05 is one of my swaps

(defn mask-seq [start]
  (lazy-seq (cons start (mask-seq (bit-shift-left start 1)))))

(defn limit-mask-seq [start]
  (take-while #(< (log2 %) 44) (mask-seq start)))

(let [connectors (swap-outputs parsed-connectors "z05" "frn")]
  (map (fn [[a b]] (incorrect-bits connectors a b)) (partition 2 1 (limit-mask-seq 3))))

;; so something broken around 5, 16, 21, 39.  that is it.
;; we have one of the swaps already.
;; let's debug 21 first since that seems easiest.

(incorrect-bits parsed-connectors (bit-set 0 21) (bit-set 0 21))

(sort-by (fn [[_ l]] (count l))
(let [connectors (swap-outputs parsed-connectors "z05" "frn")]
  (for [candidate (->> connectors
                       (filter (fn [[_ _ _ out]] (nil? (re-find #"^(x|y|z)" out))))
                       (map last))
        :let [connectors (swap-outputs connectors "z21" candidate)]]
    [candidate
     (mapcat (fn [[a b]] (incorrect-bits connectors a b)) (partition 2 1 (take 23 (limit-mask-seq 1))))])))

(expression (-> parsed-connectors (swap-outputs "z05" "frn")) "z21" 3)

;; seems gmq <-> z21 but not 100% certain
;; pin down what's going on with z16 next, return to z21 after that's fixed

;; (filter
;;  (fn [[k v]] (and (= v 1)))
;;  (test-full-system parsed-connectors (bit-set 0 21) (bit-set 0 21)))

;; (let [connectors (-> parsed-connectors
;;                      (swap-outputs "z05" "frn")
;;                      (swap-outputs "z21" "gmq"))]
;;   (map (fn [[a b]] (incorrect-bits connectors a b)) (partition 2 1 (limit-mask-seq 5))))

;; z16 def is fine -> wnf xor rsk -> wnf is fine, rsk is suspicious
;; rsk is fine, grv or tfs suspicious
;; (expression (-> parsed-connectors (swap-outputs "z05" "frn")) "z16" 4)

(filter
 (fn [[k v]] (and (= v 1) (nil? (re-find #"^x|y|z" k))))
 (test-full-system parsed-connectors 16 16))

(filter
 (fn [[k v]] (and (= v 1))) ;; (nil? (re-find #"^x|y|z" k))))
 (test-full-system parsed-connectors (bit-set 0 16) (bit-set 0 16)))

;; so rsk is broken, either itself or one of its dependencies
(incorrect-bits
 (-> parsed-connectors
     (swap-outputs "z05" "frn")
     (swap-outputs "wnf" "vtj")
     (swap-outputs "z21" "gmq"))
 (bit-set 0 38) (bit-set 0 38))

(expression (-> parsed-connectors (swap-outputs "z05" "frn")) "rwf" 1)

;; this is of course slow
(->>
 (let [connectors (-> parsed-connectors
                      (swap-outputs "z05" "frn")
                      (swap-outputs "wnf" "vtj")
                      (swap-outputs "z21" "gmq") ;; this does seem right
                      )
       candidates (->> connectors
                       (filter (fn [[_ _ _ out]] (nil? (re-find #"^(x|y)" out))))
                       (map last)
                       (remove #{"z05" "frn" "wnf" "vtj" "z21" "gmq"}))]
   (for [c1 candidates
         c2 candidates
         :when (> (compare c1 c2) 0)
         :let [connectors (swap-outputs connectors c1 c2)]]
     [[c1 c2]
      (->> (list [(bit-set 0 38) (bit-set 0 38)])
           (map (fn [[a b]] (set (incorrect-bits connectors a b))))
           ;; this short-circuits the computation if we find something wrong
           (reduce (fn [acc result]
                     (if (empty? result)
                       #{}
                       (reduced result)))))]))
 (filter (fn [[_ l]] (empty? l)))
 (println "result"))

;; OK so
()

(let [connectors  (-> parsed-connectors
                      (swap-outputs "z05" "frn")
                      (swap-outputs "wnf" "vtj")
                      (swap-outputs "z21" "gmq"))]
  (for [candidate '("wtt" "mdn")
        :let [connectors (swap-outputs connectors "z39" candidate)]]
    [["z39" candidate]
    (map (fn [[a b]] (incorrect-bits connectors a b)) (partition 2 1 (limit-mask-seq 7)))]))

(incorrect-bits
 (bit-set 0 38) (bit-set 0 38))

;; claims wnf <-> vtj should be swapped
;; options:
;; ; result ([[z40 z39] #{}] [[z39 wtt] #{}] [[z39 mdn] #{}])

;; OK, final stretch

;; "z39" "wtt"
(let [connectors  (-> parsed-connectors
                      (swap-outputs "z05" "frn")
                      (swap-outputs "wnf" "vtj")
                      (swap-outputs "z21" "gmq")
                      (swap-outputs "z39" "wtt"))]
  (map (fn [[a b]] (incorrect-bits connectors a b)) (partition 2 1 (limit-mask-seq 7))))

(defn alpha-sort []
  (string/join "," (sort compare #{"z05" "frn" "wnf" "vtj" "z21" "gmq" "z39" "wtt"})))
(alpha-sort)
