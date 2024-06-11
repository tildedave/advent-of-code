(ns advent2019.intcode
  (:require [utils :as utils]))

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
  (let [{:keys [program pc]} state]
        ;; _ (println (apply op-f (map program (map program (range (inc pc) (+ arity pc 1))))))
        ;; _ (println  (+ pc arity 1))]
    (-> state
        (assoc-in [:program (+ pc arity 1)]
                  (apply op-f (map program (map program (range (inc pc) (+ arity pc 1))))))
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

(halting-state (parse-program "1,9,10,3,2,3,11,0,99,30,40,50"))
