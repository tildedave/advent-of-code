(ns advent2016.day16
  (:require [clojure.string :as string]
            [utils :as utils]))

(set! *warn-on-reflection* true)

(defn step [^String d]
  (string/join
   [d
    "0"
    (loop [idx (dec (count d))
           sb (StringBuilder.)]
      (if (< idx 0)
        (.toString sb)
        (recur
         (dec idx)
         (.append sb (case (.charAt d idx)
                       \0 \1
                       \1 0)))))]))

(step "111100001010")

;; assumption: s has even length
(defn checksum [^String s]
  (if (= (mod (count s) 2) 1)
    (throw (Exception. "can only compute checksum for even length string"))
    (let [cs (loop [idx 0
                     sb (StringBuilder.)]
                (if
                 (= idx (count s)) (.toString sb)
                 (let [ch1 (.charAt s idx)
                       ch2 (.charAt s (inc idx))]
                   (recur (+ idx 2)
                          (.append sb (case (= ch1 ch2) true \1 false \0))))))]
      (if (= (mod (count cs) 2) 1) cs (recur cs)))))

(checksum "110010110100")

(defn answer [initial-state disk-length]
  (->
   (->> (iterate step initial-state)
       (filter #(>= (count %) disk-length))
       (first))
   (subs 0 disk-length)
   (checksum)))

(answer "10000" 20)
(answer (first (utils/read-input "2016/day16.txt")) 272)
