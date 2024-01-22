(ns advent2022.day11
  (:require [advent2022.utils :as utils]
            [clojure.string :as string]))

(def lines (utils/read-resource-lines "input/day11.txt"))

(defn parse-monkey-name [monkey-str]
  (->> monkey-str
       (re-find #"Monkey (\d+)")
       (second)
       (utils/parse-int)))

(defn parse-starting-items [monkey-str]
  (mapv utils/parse-int
        (-> (re-find #"Starting items: (\d+(, \d+)*)" monkey-str)
            (second)
            (.split ", "))))

(use 'clojure.tools.trace)
(defn parse-operation [monkey-str]
  ;; there is some incredibly cool lisp way to do this but I
  ;; guess I'm too dumb.
  (let [[arg1 op arg2]
        (->> monkey-str
             (re-find #"Operation: new = (old|\d+) (\*|\+|) (old|\d+)")
             (rest)
             (map utils/try-parse-int))]
    ;; I guess this is pretty cool.
    (fn [old + * square]
      (if (= arg1 arg2 "old")
        (square old)
        ((case op "*" * "+" +)
         (case arg1 "old" old arg1)
         (case arg2 "old" old arg2))))))

(defn parse-test-rules [monkey-str]
  (mapv #(utils/parse-int (second (re-find % monkey-str)))
       [#"Test: divisible by (\d+)"
        #"If true: throw to monkey (\d+)"
        #"If false: throw to monkey (\d+)"]))

(defn parse-monkey [monkey-str]
  (mapv #(% monkey-str) [parse-monkey-name parse-starting-items parse-operation parse-test-rules]))

(def example-monkey
  (->> lines
       (partition-by #(= % ""))
       (first)
       (string/join " ")))

(def monkeys
  (->> lines
       (partition-by #(= % ""))
       (filter #(not= (count %) 1))
       (map #(string/join " " %))
       (map parse-monkey)
       (reduce (fn [m [monkey-num & args]]
                 (assoc m monkey-num (vec args))) {})))

(defn get-first-item [monkeys monkey-num]
  (get-in monkeys [monkey-num 0 0]))

(defn remove-first-item [monkeys monkey-num]
  (update-in monkeys [monkey-num 0] (fn [v] (subvec v 1))))

(defn add-item [monkeys monkey-num item]
  (update-in monkeys [monkey-num 0] (fn [v] (conj v item))))

(get-first-item monkeys 0)

;; running a round runs, for each monkey, the items in order, and sends the
;; items forward to the other monkey.
;; so this is some kind of iteration, but we need to keep track of the current
;; monkey.
;; I suppose a recur loop is fine.

(defn process-round [[monkeys counts worry-f + * square divisible?]]
    ;; the current monkey always clears the items it has, so no need for an
    ;; intermediate loop.
    ;; however, an intermediate loop is probably easier to comprehend than
    ;; some reduce which modifies all monkey states.
  (loop [curr-monkey 0
         monkeys monkeys
         counts counts]
    (if (not (contains? monkeys curr-monkey))
      [monkeys counts worry-f + * square divisible?]
      (let [[items op test-rules] (monkeys curr-monkey)]
        (if (empty? items)
          (recur (inc curr-monkey) monkeys counts)
            ;; bulk of the logic here
            ;; remove item from current monkey.
            ;; perform operation on it.
            ;; divide its worry level by 3, round down (quot).
            ;; run test.
          (let [item (get-first-item monkeys curr-monkey)
                item (op item + * square)
                item (worry-f item)
                next-monkey (if (divisible? item (first test-rules))
                              (second test-rules)
                              (nth test-rules 2))
                next-counts (assoc counts curr-monkey (inc (if (contains? counts curr-monkey)
                                                             (counts curr-monkey)
                                                             0)))]
            (recur curr-monkey
                   (-> monkeys
                       (remove-first-item curr-monkey)
                       (add-item next-monkey item))
                   next-counts
                   )))))))

(sort > (vals {:1 2, :3 4}))

;; this is the answer to part 1
(->> (iterate process-round [monkeys {} #(quot % 3) + * #(* % %) (fn [x p] (zero? (mod x p)))])
     (take 21)
     (last)
     (second)
     (vals)
     (sort >)
     (take 2)
     (reduce *))

;; for part 2, we need to no longer keep track of ACTUAL item values and
;; instead keep track of them mod the primes that the monkeys are testing for
;; divisibility.  e.g. store modulus residue instead of actual numbers.
;; it seems like a fairly straightforward update.
;; but how much can I avoid duplicating?

(defn to-modulae [primes]
  (fn [num]
    (into {} (map #(vector % (mod num %)) primes))))

(defn op-mod-p [op p] (fn [a b] (mod (op a b) p)))
(def +-mod-p (partial op-mod-p +))
(def *-mod-p (partial op-mod-p *))

(defn op-modulae [op num x]
  (reduce (fn [num p] (update num p (partial (op p) x))) num (keys num)))
(def +-modulae (partial op-modulae +-mod-p))
(def *-modulae (partial op-modulae *-mod-p))

(defn square-modulae [num]
  (reduce (fn [num p] (update num p (fn [x] (mod (* x x) p)))) num (keys num)))

;; basic but fine enough.  (not sure I need this?)
(def modulae? map?)

(defn divisible?-modulae [num p] (zero? (num p)))

;; some test stuff
(def primes (map #(get-in % [2 0]) (vals monkeys)))
(divisible?-modulae ((to-modulae primes) 119) 23)
((to-modulae primes) 123)
(+-modulae ((to-modulae primes) 123) 12)
(*-modulae ((to-modulae primes) 123) 12)
(square-modulae ((to-modulae primes) 123))

(def modulae-monkeys
  (let [primes (map #(get-in % [2 0]) (vals monkeys))]
    (into {} (for [[k v] monkeys] [k (update v 0 (partial mapv (to-modulae primes)))]))))

;; this is the answer to part 2
(->>
 (-> (iterate process-round [modulae-monkeys {} identity +-modulae *-modulae square-modulae divisible?-modulae])
     (nth 10000)
     (nth 1)
     (vals))
 (sort >)
 (take 2)
 (reduce *))

