(ns advent2019.intcode
  (:require [utils :as utils]
            [clojure.test :refer [deftest is run-tests testing]]
            [clojure.core.match :refer [match]]
            [clojure.core.async :as a :refer [>!! <!! <!]]
            [clojure.string :as string]))

(into {} (map-indexed vector [1 2 3 4]))

(defn parse-program [^String line]
  {:program
   (->> (.split line ",")
        (map parse-long)
        (map-indexed vector)
        (into {}))
   :pc 0
   :relative-base 0})

(defn parse-file [filename]
  (->> (utils/read-input filename)
       (first)
       (parse-program)))

(defn eval-param [{:keys [program relative-base]} [mode arg]]
  (case mode
    0 (get program arg 0)
    1 arg
    2 (get program (+ relative-base arg) 0)))

(defn execute-op [op-f vlist output-register state]
  (-> state
      (assoc-in [:program output-register]
                (apply op-f vlist))))

(utils/to-digits (quot 1002 100) 3)

(def opcode-arity {1 2 2 2 3 0 4 1 5 2 6 2 7 2 8 2 9 1 99 0})
(def opcode-has-output? {1 true 2 true 3 true 7 true 8 true})

(defn step-program [state]
  (let [{:keys [program pc halted? input output relative-base default-input]} state
        opcode (mod (program pc) 100)
        arity (get opcode-arity opcode 0)
        has-output? (get opcode-has-output? opcode false)
        param-addrs (range
                     (inc pc)
                     (+ (inc pc) arity (if has-output? 1 0)))
        params (map #(get program % 0) param-addrs)
        mode-params (map
                     vector
                     (reverse (utils/to-digits (quot (program pc) 100) 3))
                     params)
        output-register (if has-output?
                          (match (last mode-params)
                            [0 addr] addr
                            [1 _] (throw (Exception. "specified immediate for output argument"))
                            [2 addr] (+ addr relative-base)))
        vlist (map
               (partial eval-param state)
               (if has-output? (drop-last mode-params) mode-params))]
    (if halted?
      state
      (let [state (case opcode
                    1 (execute-op +' vlist output-register state)
                    2 (execute-op *' vlist output-register state)
                    3 (let [i (if (nil? default-input)
                                (<!! input)
                                (first (a/alts!! [input] :default default-input)))]
                        (-> state
                            (assoc-in [:program output-register] i)))
                    4 (do
                        ;; (.println  *err* (format "sending output %d" (first vlist)))
                        (>!! output (first vlist))
                        ;; (.println *err* (format "sent output %d" (first vlist)))
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
                    9 (update
                       state
                       :relative-base
                       (partial + (first vlist)))
                    99 (assoc state :halted? true)
                    (throw
                     (Exception.
                      (format
                       "Invalid opcode: %d; PC was %d, program was %s"
                       opcode
                       pc
                       (string/join "," (->> program (sort-by first) (map second)))))))]
        (if (:jumped? state)
          (dissoc state :jumped?)
          (update state :pc (partial + 1 arity (if has-output? 1 0))))))))

(defn halting-state [program]
  (if (:halted? program)
    program
    (recur (step-program program))))

(defn run-program-internal
  ([program-or-string]
   (run-program-internal program-or-string (a/chan) (a/chan)))
  ([program-or-string input output]
   (if (string? program-or-string)
     (run-program-internal (parse-program program-or-string)
                           input output)
     (->>
      (-> program-or-string
          (assoc :input input)
          (assoc :output output)
          (halting-state)
          :program)
      (sort-by first)
      (mapv second)))))

(defn run-program
  ([program-or-string] (run-program program-or-string (a/chan) (a/chan)))
  ([program-or-string input] (run-program program-or-string input (a/chan)))
  ([program-or-string input output]
   (a/thread
     (do
       (run-program-internal program-or-string input output)
       (a/close! output)))
   output))

(defn run-file
  ([file] (run-file file (a/chan)))
  ([file input] (run-program (parse-file file) input)))

(run-program-internal "1,9,10,3,2,3,11,0,99,30,40,50")

(run-program-internal "1101,100,-1,4,0")

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
  (is (= (run-program-internal "1,9,10,3,2,3,11,0,99,30,40,50")
         [3500 9 10 70 2 3 11 0 99 30 40 50]))
  (is (= (run-program-internal "1,0,0,0,99")
         [2 0 0 0 99]))
  (is (= (run-program-internal "2,3,0,3,99")
         [2 3 0 6 99]))
  (is (= (run-program-internal "2,4,4,5,99,0")
         [2 4 4 5 99 9801]))
  (is (= (run-program-internal "1,1,1,4,99,5,6,0,99")
         [30 1 1 4 2 5 6 0 99])))

(deftest day5
  (is (= (run-program-internal "1101,100,-1,4,0")
         [1101 100 -1 4 99])
      (= (run-program-internal "1002,4,3,4,33")
         [1002, 4, 3, 4, 99])))

(defn run-with-input [program-or-string input-val]
  (let [input (a/chan)
        output (run-program program-or-string input)]
    (>!! input input-val)
    output))

(<!! (run-with-input "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
                     1))

(deftest day5-input-output
  (let [eq-8 "3,9,8,9,10,9,4,9,99,-1,8"
        lt-8 "3,9,7,9,10,9,4,9,99,-1,8"
        eq-8-immediate "3,3,1108,-1,8,3,4,3,99"
        lt-8-immediate "3,3,1107,-1,8,3,4,3,99"
        jump-test "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
        jump-test-immediate "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
        below-8-test "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"]
    (is (= 25 (<!! (run-with-input "3,0,1002,0,5,0,4,0,99" 5))))
    (is (= 1 (<!! (run-with-input eq-8 8))))
    (is (= 0 (<!! (run-with-input eq-8 4))))
    (is (= 0 (<!! (run-with-input lt-8 12))))
    (is (= 1 (<!! (run-with-input lt-8 3))))
    (is (= 1 (<!! (run-with-input eq-8-immediate 8))))
    (is (= 0 (<!! (run-with-input eq-8-immediate 4))))
    (is (= 0 (<!! (run-with-input lt-8-immediate 12))))
    (is (= 1 (<!! (run-with-input lt-8-immediate 3))))
    (is (= 1 (<!! (run-with-input jump-test 12))))
    (is (= 0 (<!! (run-with-input jump-test 0))))
    (is (= 1 (<!! (run-with-input jump-test-immediate 12))))
    (is (= 0 (<!! (run-with-input jump-test-immediate 0))))
    (is (= 999 (<!! (run-with-input below-8-test 5))))
    (is (= 1000 (<!! (run-with-input below-8-test 8))))
    (is (= 1001 (<!! (run-with-input below-8-test 15))))))

(deftest day9-test
  (testing "quine"
    (let [quine "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"]
      (is (= (mapv utils/parse-int (.split quine ","))
             (<!! (a/into [] (run-program quine)))))))
  (testing "large numbers"
    (is (= 1219070632396864 (<!! (run-program "1102,34915192,34915192,7,4,7,99,0"))))
    (is (= 1125899906842624 (<!! (run-program "104,1125899906842624,99"))))))

;; these are just utilities but helpful to have here so I don't keep copy/pasting

(defn send-string! [chan s]
  (a/onto-chan! chan (conj (mapv int s) 10) false))

(char 46)

(defn read-until-newline! [chan]
  (a/go-loop [result []]
    (let [ch (<! chan)]
      (cond
        (= ch 10) (string/join (map char result))
        (> ch 256) ch
        (nil? ch) nil
        :else (recur (conj result ch))))))

(deftest benchmarking
  (let [input (a/chan)
        output (run-program "3,100,1007,100,2,7,1105,-1,87,1007,100,1,14,1105,-1,27,101,-2,100,100,101,1,101,101,1105,1,9,101,105,101,105,101,2,104,104,101,1,102,102,1,102,102,103,101,1,103,103,7,102,101,52,1106,-1,87,101,105,102,59,1005,-1,65,1,103,104,104,101,105,102,83,1,103,83,83,7,83,105,78,1106,-1,35,1101,0,1,-1,1105,1,69,4,104,99" input)]
      (>!! input 600)
      (time (is (= 29296 (<!! output))))))

;; (run-tests 'advent2019.intcode)
