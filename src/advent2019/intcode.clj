(ns advent2019.intcode
  (:require [utils :as utils]
            [clojure.test :refer [deftest is run-tests]]
            [clojure.core.async :as a :refer [chan >!! <!!]]))

(defn parse-program [^String line]
  {:program
   (->> (.split line ",")
        (mapv utils/parse-int))
   :pc 0
   :input (chan)
   :output (chan)})

(defn parse-file [filename]
  (->> (utils/read-input filename)
       (first)
       (parse-program)))

(assoc-in {:program [1 2 3 4 5]} [:program 3] 1)

(defn val-list [state arity param-modes]
  (let [{:keys [program pc]} state
        params (map program (range (inc pc) (+ arity pc 1)))]
    (map
     (fn [[mode arg]]
       (case mode
         0 (program arg)
         1 arg))
                   ;; we add relative mode next
     (map vector param-modes params))))

(defn execute-op [op-f arity vlist state]
  (let [{:keys [program pc]} state]
    (-> state
        (assoc-in [:program (program (+ pc arity 1))]
                  (apply op-f vlist)))))

(utils/to-digits (quot 1002 100) 3)

(def opcode-arity {1 2 2 2 3 1 4 1 99 0})
(def opcode-has-output? {1 true 2 true 3 true 4 false 99 false})

(defn step-program [state]
  (let [{:keys [program pc halted? input output]} state
        opcode (mod (program pc) 100)
        arity (opcode-arity opcode)
        has-output? (opcode-has-output? opcode)
        vlist (val-list state arity (reverse (utils/to-digits (quot (program pc) 100) 3)))]
    (if halted?
      state
      (->
       (case opcode
         1 (execute-op + arity vlist state)
         2 (execute-op * arity vlist state)
         3 (let [i (<!! input)]
             (-> state
                 (assoc-in [:program (program (inc pc))] i)
                 (update :pc (partial + arity))))
         4 (do
             (>!! output (first vlist))
             state)
         99 (assoc state :halted? true))
       (update :pc (partial + 1 arity (if has-output? 1 0)))))))

(defn state-seq [program]
  (iterate step-program program))

(defn halting-state [program]
  (->> program
       (state-seq)
       (drop-while #(not (:halted? %)))
       (first)))

(defn run-program [^String line]
  (:program (halting-state (parse-program line))))

(run-program "1,9,10,3,2,3,11,0,99,30,40,50")

(run-program "1101,100,-1,4,0")

(deftest day2
  (is (= (run-program "1,9,10,3,2,3,11,0,99,30,40,50")
         [3500 9 10 70 2 3 11 0 99 30 40 50]))
  (is (= (run-program "1,0,0,0,99")
         [2 0 0 0 99]))
  (is (= (run-program "2,3,0,3,99")
         [2 3 0 6 99]))
  (is (= (run-program "2,4,4,5,99,0")
         [2 4 4 5 99 9801]))
  (is (= (run-program "1,1,1,4,99,5,6,0,99")
         [30 1 1 4 2 5 6 0 99])))

(deftest day5
  (is (= (run-program "1101,100,-1,4,0")
         [1101 100 -1 4 99])))

(run-tests 'advent2019.intcode)
