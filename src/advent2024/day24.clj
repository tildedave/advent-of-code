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
          (bit-or (bit-shift-left acc 1) z-val))
        0)))

(defn number-to-signals [num-bits number prefix]
  ;; our numbers are 45 bits so that's all we need
  (->> (range 0 (inc num-bits))
       (map (fn [n] (hash-map (format "%s%02d" prefix n) (if (bit-test number n) 1 0))))
       (reduce merge)))

(number-to-signals 4 11 "x")

(defn run-system [lines]
  (let [[initial-signals connectors] (parse-system lines)
        z-set (every-z connectors)]
    (if-let [result (->> (iterate (partial run-signals connectors) initial-signals)
                         (take 500)
                         (drop-while #(not (has-every-z? z-set %)))
                         (first))]
      (z-values z-set result)
      nil)))

(run-system (utils/read-input "2024/day24.txt"))

;; OK for part 2 we have a malfunctioning adder
;; this is a pretty cool kind of problem
;; so we need a way to figure out what kinds of numbers break
;; test a bunch of numbers
;; figure out which bits are wrong

(defn test-system [connectors x y]
  (let [z-set (every-z connectors)]
    (if-let [result (->> (iterate (partial run-signals connectors) (merge (number-to-signals 45 x "x") (number-to-signals 45 y "y")))
         (take 500)
         (drop-while #(not (has-every-z? z-set %)))
         (first))]
      (z-values z-set result)
      nil)))

(defn incorrect-bits [connectors x y]
  (let [result (test-system connectors x y)
        correct-result (+ x y)]
    (cond
      (nil? result) :no-z-output
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
(incorrect-bits parsed-connectors (bit-or 1 (bit-shift-left 0xFFFFFFFFFFF 1)) 1)
(incorrect-bits parsed-connectors 0x0000000001F 1)

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
(defn expression [connectors output]
  (letfn [(rec [output]
            (if-let [[left right connector _]
                     (->> connectors
                          (filter #(= (nth % 3) output))
                          (first))]
              [(rec left)
               connector
               (rec right)]
              output))]
    (rec output)))

(expression parsed-connectors "z03")

(for [x (range 0 17)
      y (range 0 17)
      :let [bad-bits (incorrect-bits parsed-connectors x y)]
      :when (seq bad-bits)]
  [x y bad-bits])


(defn test-full-system [connectors x y]
  (let [z-set (every-z connectors)]
    (->> (iterate (partial run-signals connectors) (merge (number-to-signals 45 x "x") (number-to-signals 45 y "y")))
         ;; just bound our computation
         (take 300)
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

(for [candidate '("frn" "ccq" "qjq")]
  [candidate (incorrect-bits (swap-outputs parsed-connectors "z05" candidate) 31 1)])

;; OK so frn <-> z05 is one of my swaps
