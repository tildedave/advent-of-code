(ns advent2015.day7
  (:require [utils :as utils]
            [clojure.set :as set]))

(defn parse-signal [line]
  (let [[_ _ loper1 oper loper2 noper litoper dest] (re-matches #"^((\w+|\d+) ([A-Z]+) (\w+|\d+)|NOT (\w+)|(\w+|\d+)) -> (\w+)$" line)]
    (cond
      noper {:operation "NOT"
             :source [noper]
             :destination dest}
      oper {:operation oper
            :source [(utils/try-parse-int loper1), (utils/try-parse-int loper2)]
            :destination dest}
      :else {:operation "LITERAL"
             :source [(utils/try-parse-int litoper)]
             :destination dest})))

(defn dependency-graph [parsed-lines]
  (reduce
   (fn [graph {:keys [source destination]}]
     (reduce
      (partial merge-with concat)
      graph
      (for [s (filter string? source)] {s [destination]})))
   {}
   parsed-lines))

;; (dependency-graph (map parse-signal (utils/read-input "2015/day7-example.txt")))

(first #{1})

(defn process-signals [parsed-lines]
  (let [by-dest (->> (map #(hash-map (:destination %) %) parsed-lines)
                     (reduce merge {}))
        graph (dependency-graph parsed-lines)]
    (loop
     [ready-nodes (->> parsed-lines
                       (filter #(and (= "LITERAL" (:operation %))
                                     (number? (first (:source %)))))
                       (map :destination)
                       (set))
      processed-nodes #{}
      unprocessed-nodes (set (map :destination parsed-lines))
      result {}]
      (if (empty? unprocessed-nodes) result
          ;; otherwise take a ready node
          (let [x (first ready-nodes)
                x-instr (by-dest x)
                processed-nodes (conj processed-nodes x)]
            (recur
             (-> ready-nodes
                 (disj x)
                 ;; now we find all the unprocessed dependencies of x
                 ;; and see if all their dependencies (as per the graph)
                 ;; are finished
                 ;; this is incredibly annoying
                 (set/union
                  (->> (graph x)
                       (remove (partial contains? processed-nodes))
                       (filter #(->> (by-dest %)
                                     (:source)
                                     (filter string?)
                                     (every? (partial contains? processed-nodes))))
                       (set))))
             processed-nodes
             (disj unprocessed-nodes x)
             (assoc result x
                    (let [[op1 op2] (:source x-instr)]
                      (case (:operation x-instr)
                        "LITERAL" (get result op1 op1)
                        "NOT" (bit-not (result op1))
                        "LSHIFT" (bit-shift-left (result op1) op2)
                        "RSHIFT" (bit-shift-right (result op1) op2)
                        "OR" (bit-or (result op1) (result op2))
                        "AND" (bit-and (get result op1 op1) (result op2)))))))))))

(defn answer-part1 [filename]
  (let [result (->> (utils/read-input (format "2015/%s" filename))
                          (map parse-signal)
                          (process-signals))
        res (get result "a")]
    (if (< res 0)
      (+ res 65536)
      res)))

(time (println (answer-part1 "day7.txt")))

(re-find #"^(\w)+ -> a$" "lx -> a")

(defn answer-part2 [filename]
  (let [a-signal (answer-part1 filename)
        result (->> (utils/read-input (format "2015/%s" filename))
                          (map #(if (re-find #"^(.*) -> b$" %)
                                  (format "%d -> b" a-signal)
                                  %))
                          (map parse-signal)
                          (process-signals))
        res (get result "a")]
    (if (< res 0)
    (+ res 65536)
    res)))

(answer-part2 "day7.txt")
