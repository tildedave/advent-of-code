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

;; so instead of these approaches we will just use a map.
;; template-pairs will be used, but just for the first.

(defn start [template]
  (reduce
   (fn [m p] (assoc m p 1))
   {}
   (template-pairs template)))

(defn step-better [template rules]
  (->> template
       (reduce
        (fn [l [p n]]
          (let [ch (rules p)]
            (-> l
                (conj {(str (first p) ch) n})
                (conj {(str ch (second p)) n}))))
        [])
       (apply merge-with +)))

(let [[template rules] (parse-input (utils/read-resource-lines "input/day14-example.txt"))]
  (step-better (start template) rules))

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
    (iterate #(step-better % rules) (start template))))

(defn step-seq-brute-force [lines]
  (let [[template rules] (parse-input lines)]
    (iterate #(step % rules) template)))

(defn char-frequency-brute-force [str]
  (reduce
   (fn [m ch] (update m ch (fnil inc 0)))
   {}
   str))

(reduce + (vals (nth (step-seq (utils/read-resource-lines "input/day14.txt")) 10)))

(defn char-frequency [pair-count start-template]
  (->
   (reduce
    (fn [m [pair n]]
      (-> m
          (update (first pair) (fnil (partial + n) 0))
          (update (second pair) (fnil (partial + n) 0))))
       ;; we double-count every char except for the first and last,
       ;; which never change
    (merge-with +
                {(first start-template) 1}
                {(last start-template) 1})
    pair-count)
   (update-vals #(/ % 2))))

(-> (step-seq-brute-force (utils/read-resource-lines "input/day14.txt"))
    (nth 0)
    (char-frequency-brute-force))

(-> (step-seq (utils/read-resource-lines "input/day14.txt"))
    (nth 0)
    (char-frequency (first (parse-input (utils/read-resource-lines "input/day14.txt")))))

(defn score [frequencies]
  (let [sorted-frequencies (sort-by second frequencies)
        least-freq (second (first sorted-frequencies))
        most-freq (second (last sorted-frequencies))]
    (- most-freq least-freq)))

(defn answer [lines n]
  (let [[template _] (parse-input lines)]
    (-> lines
        (step-seq)
        (nth n)
        (char-frequency template)
        (score))))

(defn answer-part1 [lines] (answer lines 10))
(answer-part1 (utils/read-resource-lines "input/day14-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day14.txt"))

(defn answer-part2 [lines] (answer lines 40))
(answer-part2 (utils/read-resource-lines "input/day14-example.txt"))
(answer-part2 (utils/read-resource-lines "input/day14.txt"))

    ;;     sorted-frequencies (sort-by second frequencies)
    ;;     least-freq (second (first sorted-frequencies))
    ;;     most-freq (second (last sorted-frequencies))]
    ;; (- most-freq least-freq)))

(answer-part1 (utils/read-resource-lines "input/day14-example.txt"))
