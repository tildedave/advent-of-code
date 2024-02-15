(ns advent2021.day14
  (:require [advent2021.utils :as utils]
            [clojure.string :as string]))


(defn parse-input [lines]
  (let [[[template] insertion-rules]
        (->> lines
             (partition-by (partial = ""))
             (remove (partial = (list ""))))]
    [template (->> insertion-rules
                   (map #(.split % "->"))
                   (map #(mapv string/trim %))
                   (into {}))]))

(defn template-pairs
  ([template] (template-pairs template 0))
  ([template idx]
   (if (>= (inc idx) (count template)) nil
       (lazy-seq (cons (.substring template idx (+ 2 idx))
                       (template-pairs template (inc idx)))))))

(defn step [template rules]
  (let [pairs (template-pairs template)
        new-letters (map rules pairs)]
    (->> new-letters
         (map-indexed vector)
         (map (fn [[n x]] [(inc (* 2 n)) x]))
         (reduce
          (fn [s [n x]]
            (str (.substring s 0 n) x (.substring s n)))
          template))))

(defn step-seq [lines]
  (let [[template rules] (parse-input lines)]
    (iterate #(step % rules) template)))

(defn char-frequency [str]
  (reduce
   (fn [m ch] (update m ch (fnil inc 0)))
   {}
   str))

(defn answer-part1 [lines]
  (let [frequencies (-> lines
                        (step-seq)
                        (nth 10)
                        (char-frequency))
        sorted-frequencies (sort-by second frequencies)
        least-freq (second (first sorted-frequencies))
        most-freq (second (last sorted-frequencies))]
    (- most-freq least-freq)))

(answer-part1 (utils/read-resource-lines "input/day14-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day14.txt"))
