(ns advent2019.intcode
  (:require [utils :as utils]
            [clojure.test :refer [deftest is run-tests]]))

(defn parse-program [^String line]
  {:program
   (->> (.split line ",")
        (mapv utils/parse-int))
   :pc 0})

(defn parse-file [filename]
  (->> (utils/read-input filename)
       (first)
       (parse-program)))

(assoc-in {:program [1 2 3 4 5]} [:program 3] 1)

(defn execute-op [op-f arity state]
  (let [{:keys [program pc]} state
        params (map program (range (inc pc) (+ arity pc 1)))
        ;; for now the vals are just registers, this changes eventually
        vals (map program params)]
    (-> state
        (assoc-in [:program (program (+ pc arity 1))]
                  (apply op-f vals))
        (update :pc (partial + arity 2)))))

(defn step-program [state]
  (let [{:keys [program pc halted?]} state
        opcode (program pc)]
    (if halted?
      state
      (case opcode
        1 (execute-op + 2 state)
        2 (execute-op * 2 state)
        99 (assoc state :halted? true)))))

(defn state-seq [program]
  (iterate step-program program))

(defn halting-state [program]
  (->> program
       (state-seq)
       (drop-while #(not (:halted? %)))
       (first)))

(defn run-program [^String line]
  (:program (halting-state (parse-program line))))

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

(run-tests 'advent2019.intcode)
