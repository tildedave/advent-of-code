(ns advent2016.day23
  (:require [utils :as utils]))

;; copied from day 12 for now.

(defn parse-instruction [s]
  (let [[s1 s2 s3] (.split s " ")]
    (case s1
      "cpy" [:copy (utils/try-parse-int s2) s3]
      "inc" [:inc s2]
      "dec" [:dec s2]
      "jnz" [:jnz (utils/try-parse-int s2) (utils/try-parse-int s3)]
      "tgl" [:toggle s2])))

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
                 state]))
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

(full-exec "2016/day23-example.txt")

;; OK so this thing is computing factorials
;; and it looks like it only runs the toggle 5 times.
;; after the toggles run, a has 7! in its register
;; then we add 81 * 73 to it.


(->> (program-seq "2016/day23.txt" {"a" 7})
     (map (fn [[program n state]] [(get program n) n state]))
     (map-indexed vector)
     (drop 139000))

;; (full-exec "2016/day23.txt")

(take 9 (iterate exec (initial-state "2016/day23-example.txt")))
