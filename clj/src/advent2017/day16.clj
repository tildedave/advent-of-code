(ns advent2017.day16
   (:require [utils :as utils]
             [clojure.set :refer [map-invert]]))

 (map vector (range 0 10) (range 0 3))
 (first "a")
 (defn parse-instr [s]
   (if-let [[_ a] (re-matches #"^s(\d+)$" s)]
     [:spin (utils/parse-int a)]
     (if-let [[_ a b] (re-matches #"^x(\d+)/(\d+)$" s)]
       [:exchange (utils/parse-int a) (utils/parse-int b)]
       (if-let [[_ a b] (re-matches #"^p(\w)/(\w)" s)]
         [:partner (first a) (first b)]
         (throw (Exception. (format "could not parse %s" s)))))))

 (defn step [state [instr a b]]
   (case instr
     :spin (let [l (count state)
                 spin-start (- l a)]
             (reduce
              (fn [acc [old new]]
                (assoc acc new (state old)))
              state
              (map
               (fn [n]
                 (let [new (mod (- n spin-start) l)]
                   [n (if (< new 0) (+ new l) new)]))
               (range 0 l))))
     :exchange (-> state
                   (assoc a (state b))
                   (assoc b (state a)))
     :partner (let [[a-idx] (->> state
                                 (filter (fn [[_ a']] (= a' a)))
                                 (first))
                    [b-idx] (->> state
                                 (filter (fn [[_ b']] (= b' b)))
                                 (first))]
                (-> state
                    (assoc a-idx b)
                    (assoc b-idx a)))))

 (defn expand [s] (into {} (map-indexed vector (seq s))))
 (defn contract [state]
   (apply str (map #(get state %) (range 0 (count state)))))

 (->> (list "s1" "x3/4" "pe/b")
      (map parse-instr)
      (reduce step (expand "abcde"))
      (contract))

 (->> (utils/read-input "2017/day16.txt")
      (first)
      (#(.split % ","))
      (map parse-instr)
      (reduce step (expand "abcdefghijklmnop"))
      (contract))

 (defn answer-part2 []
   (let [ilist
         (->> (utils/read-input "2017/day16.txt")
              (first)
              (#(.split % ","))
              (map parse-instr))
         v (->> (range 0 1000000000)
                (reductions
                 (fn [state n] (reduce step state ilist))
                 (expand "abcdefghijklmnop"))
                (map contract)
                (map-indexed vector))
         [start length seen]
         (reduce
          (fn [seen [n value]]
            (if (contains? seen value)
                (reduced [n (- n (seen value)) seen])
              (assoc seen value n))) {} v)
         big-length 1000000000
         seen (map-invert seen)]
     (seen (mod (- big-length start) length))))

(answer-part2)
