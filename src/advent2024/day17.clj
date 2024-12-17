(ns advent2024.day17
  (:require
    [clojure.string :as string]
    [utils :as utils]))

(def instruction
  {0 :adv ;; A register / 2^(combo)
   1 :bxl ;; bitxise XOR of B with literal
   2 :bst ;; value of combo operator mod 8, storing to B
   3 :jnz ;; does nothing if A = 0, otherwise IP -> literal
   4 :bxc ;; bitwise XOR of B and C, stores result in B (reads operand and does nothing)
   5 :out ;; calculates value of combo mod 8 and outputs it
   6 :bdv ;; B register / 2^(combo)
   7 :cdv ;; C register / 2^(combo)
   })

(defn combo-operand-value [state operand]
  (case operand
    0 0
    1 1
    2 2
    4 (state :a)
    5 (state :b)
    6 (state :c)
    7 (assert false "7 is an invalid value")))

(double (/ 21 8))
(bit-shift-right 21 3)

(defn dv [state register operand]
  ;; I think this is a bit shift but let's be careful
;;   (println (bit-shift-right (state :a) (combo-operand-value state operand))
;;            "vs"
;;            (quot (state :a) (combo-operand-value state operand)
;;            ))
;;   (println (state :a) (combo-operand-value state operand))
  (assert (=
         (bit-shift-right (state :a) (combo-operand-value state operand))
         (quot (state :a) (bit-shift-left 1 (combo-operand-value state operand)))))
  (-> state
      (assoc register (bit-shift-right (state :a) (combo-operand-value state operand)))
      (update :ip (partial + 2))))

(defn execute [{:keys [a b c ip program output] :as state}]
  (let [opcode (get program ip)
        operand (get program (inc ip))]
    (case (instruction opcode)
      :adv (dv state :a operand)
      :bxl
      (-> state
          (update :b bit-xor operand)
          (update :ip (partial + 2)))
      :bst
      (-> state
          (assoc :b (mod (combo-operand-value state operand) 8))
          (update :ip (partial + 2)))
      :jnz
      (if (zero? a)
        (update state :ip (partial + 2))
        (assoc state :ip operand))
      :bxc
      (-> state
          (update :b (partial bit-xor c))
          (update :ip (partial + 2)))
      :out
      (-> state
          (update :output #(conj % (mod (combo-operand-value state operand) 8)))
          (update :ip (partial + 2)))
      :bdv (dv state :b operand)
      :cdv (dv state :c operand))))

(def new-state {:a 0 :b 0 :c 0 :ip 0 :program [] :output []})


(defn program-seq [state]
  (take-while #(not= % :halt)
              (iterate #(if (contains? (:program %) (:ip %))
                          (execute %)
                          :halt)
                       state)))

;; may infinite loop :-)
(defn fully-execute [state] (last (program-seq state)))

(program-seq (-> new-state (assoc :c 9) (assoc :program [2 6])))

(assert (= 1
           (-> new-state (assoc :c 9) (assoc :program [2 6]) fully-execute :b)))
(assert (= [0 1 2]
           (-> new-state (assoc :a 10) (assoc :program [5 0 5 1 5 4])
               fully-execute
               :output)))

(assert (= [4 2 5 6 7 7 7 7 3 1 0]
           (-> new-state
               (assoc :a 2024)
               (assoc :program [0 1 5 4 3 0])
               fully-execute
               :output)))

(assert (= 26
           (-> new-state
               (assoc :b 29)
               (assoc :program [1 7])
               fully-execute
               :b)))

(assert (= 44354
           (-> new-state
               (assoc :b 2024)
               (assoc :c 43690)
               (assoc :program [4 0])
               fully-execute
               :b)))

(-> new-state (assoc :a 729) (assoc :program [0 1 5 4 3 0])
    fully-execute
    :output
    (#(string/join "," %)))

(let [[[a] [b] [c] _ program] (map utils/str->nums (utils/read-input "2024/day17.txt"))]
  (-> new-state
      (assoc :a a)
      (assoc :b b)
      (assoc :c c)
      (assoc :program (vec program))))
