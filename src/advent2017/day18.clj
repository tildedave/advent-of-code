(ns advent2017.day18
  (:require [utils :as utils]
            [clojure.core.async :as a :refer [>!! <!!]]))

(defn parse-instr [line]
  (if-let [[_ op arg1 _ arg2] (re-matches #"^(snd|set|add|mul|mod|rcv|jgz) (-?\w+)( (-?\w+))?$" line)]
    [(keyword op) (utils/try-parse-int arg1) (utils/try-parse-int arg2)]
    (throw (Exception. (format "Could not parse %s" line)))))

(defn step [program]
  (fn [state]
    (let [{:keys [last-sound registers pc]} state]
      (if-let [[instr x y] (get program pc)]
        (let [resolve (fn [x]
                        (cond (nil? x) nil
                              (number? x) x
                              :else (get registers x 0)))]
          (case instr
            :snd (-> state
                     (assoc :last-sound (resolve x))
                     (update :pc inc))
            :set (-> state
                     (assoc-in [:registers x] (resolve y))
                     (update :pc inc))
            :add (-> state
                     (assoc-in [:registers x] (+ (resolve x) (resolve y)))
                     (update :pc inc))
            :mul (-> state
                     (assoc-in [:registers x] (* (resolve x) (resolve y)))
                     (update :pc inc))
            :mod (-> state
                     (assoc-in [:registers x] (mod (resolve x) (resolve y)))
                     (update :pc inc))
            :rcv (if (zero? (resolve x))
                   (update state :pc inc)
                   (-> state
                       (assoc :recovered-frequency last-sound)
                       (update :pc inc)))
            :jgz (if (> (resolve x) 0)
                   (update state :pc (partial + (resolve y)))
                   (update state :pc inc))))
        state))))

(def initial-state {:pc 0 :registers {} :last-sound 0})
(parse-instr "snd 5")

(iterate
 (step  (mapv parse-instr (utils/read-input "2017/day18.txt")))
 initial-state)

(reduce
 (fn [acc {:keys [recovered-frequency]}]
   (if (or (nil? recovered-frequency) (zero? recovered-frequency))
     acc
     (reduced recovered-frequency)))
 (iterate
  (step  (mapv parse-instr (utils/read-input "2017/day18.txt")))
  initial-state))

(defn step2 [program]
  (fn [[state1 state2]]))

(defn step-p2 [program input-chan output-chan]
  (fn [state]
    (let [{:keys [last-sound registers pc]} state]
      (if-let [[instr x y] (get program pc)]
        (let [resolve (fn [x]
                        (cond (nil? x) nil
                              (number? x) x
                              :else (get registers x 0)))]
          (case instr
            :set (-> state
                     (assoc-in [:registers x] (resolve y))
                     (update :pc inc))
            :add (-> state
                     (assoc-in [:registers x] (+ (resolve x) (resolve y)))
                     (update :pc inc))
            :mul (-> state
                     (assoc-in [:registers x] (* (resolve x) (resolve y)))
                     (update :pc inc))
            :mod (-> state
                     (assoc-in [:registers x] (mod (resolve x) (resolve y)))
                     (update :pc inc))
            :jgz (if (> (resolve x) 0)
                   (update state :pc (partial + (resolve y)))
                   (update state :pc inc))))
        state))))

(def printer (agent nil))

(defn run-part2 [program]
  (let [a-input  (a/chan 255)
        b-input (a/chan 255)]
    (println "launch 1")
    (doseq [p (list 0 1)]
      (a/go-loop [state (-> initial-state
                            (assoc-in [:registers "p"] p)
                            (assoc :num-sends 0))]
        (send printer (fn [s] (println
                               "p" p
                               "pc" (:pc state)
                               "instr" (get program (:pc state))
                               "registers" (:registers state))))
      ;; (println "p" p)
        ;; (println "p" p "instr" (get program (:pc state)))
        (if-let [[instr x y] (get program (:pc state))]
          (recur
           (let [resolve (fn [x]
                           (cond (nil? x) nil
                                 (number? x) x
                                 :else (get-in state [:registers x])))]
             (case instr
               :snd (do
                      (>!! (case p
                                0 b-input
                                1 a-input) (resolve x))
                      ;; (if (= p 1)
                      (send printer (fn [s] (println "*****" (inc (:num-sends state)) p "*****")))
                      (-> state
                          (update :pc inc)
                          (update :num-sends inc)))
               :rcv (let [q (<!! (case p 0 a-input 1 b-input))
                          _ (send printer (fn [s] (println "received" q "p" p)))]
                      (-> state
                        (assoc [:registers x] q)
                        (update state :pc inc)))
               :set (-> state
                        (assoc-in [:registers x] (resolve y))
                        (update :pc inc))
               :add (-> state
                        (assoc-in [:registers x] (+ (resolve x) (resolve y)))
                        (update :pc inc))
               :mul (-> state
                        (assoc-in [:registers x] (* (resolve x) (resolve y)))
                        (update :pc inc))
               :mod (-> state
                        (assoc-in [:registers x] (mod (resolve x) (resolve y)))
                        (update :pc inc))
               :jgz (if (> (resolve x) 0)
                      (update state :pc (partial + (resolve y)))
                      (update state :pc inc)))))
          state)))
      (println "sleeping")
      (Thread/sleep 30000)
      (a/close! b-input)
      (a/close! a-input)))

(doseq []
 (run-part2 (mapv parse-instr (utils/read-input "2017/day18.txt"))))
