(ns advent2023.day20
  (:require [clojure.set :as set]
            [utils :as utils]))

;; this is a reverse engineering problem - alas, I already reverse engineered
;; it once.

;; push-button sends a queue

(def example1
  '("broadcaster -> a, b, c"
    "%a -> b"
    "%b -> c"
    "%c -> inv"
    "&inv -> a"))

(def example2
  '("broadcaster -> a"
    "%a -> inv, con"
    "&inv -> b"
    "%b -> con"
    "&con -> output"))

(defn parse-line [line]
  (let [[type source ^String dest-string]
        (->> line
             (re-matches #"^(%|&|)(\w+) -> (.*)$")
             (rest))
        outs (.split dest-string ", ")]
    [{source outs}
     (reduce merge (for [out outs] {out #{source}}))
     {source (case type
               "%" :flip-flop
               "&" :conjunction
               (if (= source "broadcaster") :broadcaster
                   (throw (Exception. "Unable to determine type"))))}]))

(defn parse-machine [lines]
  (let [[out-connections in-connections register-types]
        (->> (map parse-line lines)
             (reduce
              (fn [[acc-out-connections acc-in-connections acc-types]
                   [out-connections in-connections types]]
                [(merge acc-out-connections out-connections)
                 (merge-with set/union acc-in-connections in-connections)
                 (merge acc-types types)])))]
    {:out-connections out-connections
     :conjunctions
     (->> (keys out-connections)
          (filter #(= (register-types %) :conjunction))
          (map #(hash-map % (reduce merge (map (fn [c] {c :low}) (in-connections %)))))
          (reduce merge))
     :flip-flops
     (->> (keys out-connections)
          (filter #(= (register-types %) :flip-flop))
          (map #(hash-map % :low))
          (reduce merge))
     :register-types register-types}))

(parse-machine example2)

(defn invert-pulse [pulse]
  (case pulse
    :low :high
    :high :low))

(defn send-pulse [machine]
  (let [{:keys [out-connections flip-flops
                conjunctions register-types]} machine]
    (loop [queue [["broadcaster" "button" :low]]
           flip-flops flip-flops
           conjunctions conjunctions
           all-signals []]
      (if (empty? queue)
      ;; TODO: add count of signals or something
        (-> machine
            (assoc :flip-flops flip-flops)
            (assoc :conjunctions conjunctions)
            (assoc :all-signals all-signals))
        (let [[pulse-dest pulse-from pulse-strength] (first queue)
              queue (subvec queue 1)
              all-signals (conj all-signals [pulse-dest pulse-from pulse-strength])]
          (case (register-types pulse-dest)
            :broadcaster
            (recur
             (into queue (map #(vector % pulse-dest pulse-strength) (out-connections pulse-dest)))
             flip-flops
             conjunctions
             all-signals)
            :flip-flop
            (case pulse-strength
              :high ;; nothing
              (recur queue flip-flops conjunctions all-signals)
              :low
              (recur
               (into queue (map #(vector % pulse-dest (invert-pulse (flip-flops pulse-dest))) (out-connections pulse-dest)))
               (update flip-flops pulse-dest invert-pulse)
               conjunctions
               all-signals))
            :conjunction
            (let [next-conjunctions (assoc-in conjunctions [pulse-dest pulse-from] pulse-strength)
                  signal (if (every? (partial = :high) (vals (next-conjunctions pulse-dest)))
                           :low
                           :high)]
              (recur
               (into queue (map #(vector % pulse-dest signal) (out-connections pulse-dest)))
               flip-flops
               next-conjunctions
               all-signals))
            (recur queue flip-flops conjunctions all-signals)))))))

(send-pulse (parse-machine example2))

(defn total-signals [lines num-presses]
  (->> (map :all-signals (iterate send-pulse (parse-machine lines)))
       (rest)
       (take num-presses)
       (map #(as-> % m
                  (group-by (fn [[_ _ pulse-strength]] pulse-strength) m)
                  (update-vals m count)))
       (reduce (partial merge-with +))
       (vals)
       (reduce *)))

;; correct
(total-signals (utils/read-input "2023/day20.txt") 1000)

;; OK so my machine has four important registers, all need to be
;; HIGH to work.
;; tr, dr, nh, xm

(defn cycles [lines]
  (->> (iterate send-pulse (parse-machine lines))
       (take 10000)
       (map :all-signals)
       (map-indexed
        (fn [n l] [n (filter (fn [[pulse-dest _ pulse-strength]]
                               (and (= pulse-dest "dh")
                               (= pulse-strength :high))) l)]))
       (remove (fn [[n l]] (empty? l)))
       (take 4)
       (map first)
       (reduce utils/lcm)
       ))

;; part 2 answer
(cycles (utils/read-input "2023/day20.txt"))
