(ns advent2016.assembunny
  (:require [utils :as utils]))

(defn parse-instruction [s]
  (let [[s1 s2 s3] (.split s " ")]
    (case s1
      "cpy" [:copy (utils/try-parse-int s2) s3]
      "inc" [:inc s2]
      "dec" [:dec s2]
      "jnz" [:jnz (utils/try-parse-int s2) (utils/try-parse-int s3)]
      "tgl" [:toggle s2]
      "out" [:out s2])))

(defn exec [[program n state]]
  (if-let [[instr x y] (get program n)]
    (case instr
      :copy [program
             (inc n)
             (if (number? x)
               (assoc state y x)
               (assoc state y (state x)))]
      :inc [program (inc n) (update state x inc)]
      :dec [program (inc n) (update state x dec)]
      :jnz (if (zero? (if (number? x) x (state x)))
             [program
              (inc n)
              state]
             [program
              (+ n (if (number? y) y (state y)))
              state])
      :toggle (let [idx (+ n (state x))]
                [(if (contains? program idx)
                   (update program idx (fn [[instr x y]]
                                         (case instr
                                           :copy [:jnz x y]
                                           :inc [:dec x]
                                           :dec [:inc x]
                                           :jnz [:copy x y]
                                           :toggle [:inc x])))
                   program)
                 (inc n)
                 state])
      ;; we won't actually do anything with "out", but we can use
      ;; our program-seq to understand the patterns.
      :out [program (inc n) state])
    [program n state]))

(defn initial-state [filename]
  (let [program (->> (utils/read-input filename)
                     (mapv parse-instruction))]
    [program 0 {"a" 0 "b" 0 "c" 0 "d" 0}]))


(defn program-seq
  ([filename] (program-seq filename {}))
  ([filename merge-state]
   (iterate exec (update (initial-state filename) 2 #(merge % merge-state)))))

(defn full-exec
  ([filename] (full-exec filename {}))
  ([filename merge-state]
   (reduce (fn [_ [program n state]]
             (if (get program n) nil (reduced state)))
           (program-seq filename merge-state))))
