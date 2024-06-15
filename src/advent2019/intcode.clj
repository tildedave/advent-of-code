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

(defn execute-op [op-f vlist output-register state]
  (-> state
      (assoc-in [:program output-register]
                (apply op-f vlist))))

(utils/to-digits (quot 1002 100) 3)

(def opcode-arity {1 2 2 2 3 0 4 1 5 2 6 2 7 2 8 2 99 0})
(def opcode-has-output? {1 true 2 true 3 true 7 true 8 true})

(defn step-program [state]
  (let [{:keys [program pc halted? input output]} state
        opcode (mod (program pc) 100)
        arity (get opcode-arity opcode 0)
        has-output? (get opcode-has-output? opcode false)
        output-register (if has-output? (program (+ pc arity 1)) -1)
        vlist (val-list state arity (reverse (utils/to-digits (quot (program pc) 100) 3)))]
    (if halted?
      state
      (let [state (case opcode
        1 (execute-op + vlist output-register state)
        2 (execute-op * vlist output-register state)
        3 (let [i (<!! input)]
            (-> state
                (assoc-in [:program (program (inc pc))] i)))
        4 (do
            (>!! output (first vlist))
            state)
        5 (if (not= (first vlist) 0)
            (-> state
                (assoc :pc (second vlist))
                (assoc :jumped? true))
            state)
        6 (if (zero? (first vlist))
            (-> state
                (assoc :pc (second vlist))
                (assoc :jumped? true))
            state)
        7 (assoc-in
           state
           [:program output-register]
           (if (< (first vlist) (second vlist)) 1 0))
        8 (assoc-in
           state
           [:program output-register]
           (if (= (first vlist) (second vlist)) 1 0))
        99 (assoc state :halted? true)
        (throw (Exception. (format "Invalid opcode: %d" opcode))))]
      (if (:jumped? state)
        (dissoc state :jumped?)
        (update state :pc (partial + 1 arity (if has-output? 1 0))))))))

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
;; (let [input (a/chan)
;;       output (a/chan)]
;;   (a/thread (run-program "3,0,1002,0,5,0,4,0,99" input output))
;;   (>!! input 5)
;;   (assert (= 25 (<!! output)))
;;   (a/close! input)
;;   (a/close! output))

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

(defn run-input-output [program-or-string input-val]
  (let [input (a/chan)
        output (a/chan)]
    (a/thread (run-program program-or-string input output))
    (>!! input input-val)
    output))

(<!! (run-input-output "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
                  1))

(deftest day5-input-output
  (let [eq-8 "3,9,8,9,10,9,4,9,99,-1,8"
        lt-8 "3,9,7,9,10,9,4,9,99,-1,8"
        eq-8-immediate "3,3,1108,-1,8,3,4,3,99"
        lt-8-immediate "3,3,1107,-1,8,3,4,3,99"
        jump-test "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
        jump-test-immediate "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
        below-8-test "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"]
    (is (= 25 (<!! (run-input-output "3,0,1002,0,5,0,4,0,99" 5))))
    (is (= 1 (<!! (run-input-output eq-8 8))))
    (is (= 0 (<!! (run-input-output eq-8 4))))
    (is (= 0 (<!! (run-input-output lt-8 12))))
    (is (= 1 (<!! (run-input-output lt-8 3))))
    (is (= 1 (<!! (run-input-output eq-8-immediate 8))))
    (is (= 0 (<!! (run-input-output eq-8-immediate 4))))
    (is (= 0 (<!! (run-input-output lt-8-immediate 12))))
    (is (= 1 (<!! (run-input-output lt-8-immediate 3))))
    (is (= 1 (<!! (run-input-output jump-test 12))))
    (is (= 0 (<!! (run-input-output jump-test 0))))
    (is (= 1 (<!! (run-input-output jump-test-immediate 12))))
    (is (= 0 (<!! (run-input-output jump-test-immediate 0))))
    (is (= 999 (<!! (run-input-output below-8-test 5))))
    (is (= 1000 (<!! (run-input-output below-8-test 8))))
    (is (= 1001 (<!! (run-input-output below-8-test 15))))))

(run-tests 'advent2019.intcode)
