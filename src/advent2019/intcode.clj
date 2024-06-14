(ns advent2019.intcode
  (:require [utils :as utils]
            [clojure.test :refer [deftest is run-tests]]
            [clojure.core.async :as a :refer [>!! <!!]]))

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

(def opcode-arity {1 2 2 2 3 0 4 1 99 0})
(def opcode-has-output? {1 true 2 true 3 true 4 false 99 false})

(defn step-program [state]
  (let [{:keys [program pc halted? input output]} state
        opcode (mod (program pc) 100)
        arity (get opcode-arity opcode 0)
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
                 (assoc-in [:program (program (inc pc))] i)))
         4 (do
             (>!! output (first vlist))
             state)
         99 (assoc state :halted? true)
         (throw (Exception. (format "Invalid opcode: %d" opcode))))
       (update :pc (partial + 1 arity (if has-output? 1 0)))))))

(defn halting-state [program]
  (if (:halted? program)
    program
    (recur (step-program program))))

(defn run-program
  ([program-or-string]
   (run-program program-or-string (a/chan) (a/chan)))
  ([program-or-string input output]
   (if (string? program-or-string)
     (run-program (parse-program program-or-string)
                  input output)
     (-> program-or-string
         (assoc :input input)
         (assoc :output output)
         (halting-state)
         :program))))

(run-program "1,9,10,3,2,3,11,0,99,30,40,50")

(run-program "1101,100,-1,4,0")
/
;; works
(let [input (a/chan)
      output (a/chan)]
  (a/thread (run-program "3,0,1002,0,5,0,4,0,99" input output))
  (>!! input 5)
  (assert (= 25 (<!! output)))
  (a/close! input)
  (a/close! output))

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
         [1101 100 -1 4 99])
      (= (run-program "1002,4,3,4,33")
         [1002, 4, 3, 4, 99])))

(deftest day5-input-output
  (let [input (a/chan)
        output (a/chan)]
    (a/thread (run-program "3,0,1002,0,5,0,4,0,99" input output))
    (>!! input 5)
    (is (= 25 (<!! output)))
    (a/close! input)
    (a/close! output)))

(run-tests 'advent2019.intcode)
