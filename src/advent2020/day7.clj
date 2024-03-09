(ns advent2020.day7
  (:require [utils :as utils]))

;; (def bag-re #"^([\w\s]+) bags contain (no other bags|((, )?(1 ([\w\s]+) bag|(\d ([\w\s]+) bags)))+).$")

(def bag-re #"^((?:\s?\w+)+) bags contain (.*)\.$")
(def individual-bag-re
  #"(?:, )?(\d) ((?:\s?\w+)+) bags?(.*)")

(re-find individual-bag-re "1 bright white bag, 2 muted yellow bags")

(defn parse-bag [line]
  (let [[bag-type rest-line] (rest (re-matches bag-re line))]
    {bag-type
     (loop
      [rest-line rest-line
       contents {}]
       (cond
         (empty? rest-line) contents
         (= rest-line "no other bags") contents
         :else
         (let [[bag-count bag-type rest-line] (rest (re-find individual-bag-re rest-line))]
           (recur
            rest-line
            (assoc contents bag-type (utils/parse-int bag-count))))))}))

(->> (utils/read-input "2020/day7-example.txt")
     (map parse-bag)
     (reduce into {}))

(hash-map 1 2)


(let [adj-map (->> (utils/read-input "2020/day7-example.txt")
                   (map parse-bag)
                   (reduce into {})
                   (map (fn [[k v-list]] (map #(hash-map (first %) (list k)) v-list)))
                   (flatten)
                   (apply merge-with concat))]
  (loop [queue ["shiny gold"]
         valids #{}]
    (if (empty? queue) valids
        (let [x (first queue)]

          )
        )


