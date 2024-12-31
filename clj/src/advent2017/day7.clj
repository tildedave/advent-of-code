(ns advent2017.day7
  (:require [utils :as utils]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)

(def line-re #"(\w+) \((\d+)\)( -> (.*))?")

(re-matches line-re "pbga (66)")
(re-matches line-re "fwft (72) -> ktlj, cntj, xhth")

(defn parse-line [line]
  (if-let [[_ node num _ ^String conns] (re-matches line-re line)]
    {node {:num (utils/parse-int num)
           :conns (if conns (set (.split conns ", ")) #{})}}
    (throw (Exception. (format "Could not parse %s" line)))))

(parse-line "pbga (66)")
(parse-line "fwft (72) -> ktlj, cntj, xhth")

(defn parse-tree [lines]
  (->> lines
       (map parse-line)
       (reduce merge)))

(parse-tree (utils/read-input "2017/day7-example.txt"))

(defn topo-sort [adj]
  (let [empty-map (reduce merge (for [node (keys adj)] {node #{}}))]
    (reduce
     (fn [[outgoing-edges incoming-edges] [node {:keys [num conns]}]]
       (reduce
        (fn [[outgoing-edges incoming-edges] conn]
          [(merge-with set/union outgoing-edges {node #{conn}})
           (merge-with set/union incoming-edges {conn #{node}})])
        [outgoing-edges incoming-edges]
        conns))
     [empty-map empty-map]
     adj)))

(parse-tree (utils/read-input "2017/day7-example.txt"))

(defn starting-node [adj]
   (let [[_ incoming] (topo-sort adj)]
    (->> incoming
         (filter (fn [[_ s]] (zero? (count s))))
         (first)
         (first))))

(defn answer-part1 [lines]
  (starting-node (parse-tree lines)))

(answer-part1 (utils/read-input "2017/day7-example.txt"))
(answer-part1 (utils/read-input "2017/day7.txt"))

(defn node-weight [adj node]
  ;; and the weight of its sub-eges
  (reduce
   +
   (get-in adj [node :num])
   ;; this is inefficient, I could be smarter via a recursive memoized
   ;; function. it may matter
   (map (partial node-weight adj) (get-in adj [node :conns]))))

;; (confirmed that it does not matter.)
;; (let [adj (parse-tree (utils/read-input "2017/day7.txt"))]
;;   (map (partial node-weight adj) (keys adj)))

(defn answer-part2 [lines]
  (let [adj (parse-tree lines)]
    (loop
     [node (starting-node adj)
      wanted-weight 0]
      (let [children-by-weight
            (->> (get-in adj [node :conns])
                 (map #(vector % (node-weight adj %)))
                 (sort-by second)
                 (partition-by second))]
        (if (= (count children-by-weight) 1)
          (- wanted-weight (- (node-weight adj node) (get-in adj [node :num])))
          (let [outlier-node (->> children-by-weight
                                  (filter #(= (count %) 1))
                                  (first)
                                  (first)
                                  (first))
                desired-weight (->> children-by-weight
                                    (remove #(= (count %) 1))
                                    (first)
                                    (first)
                                    (second)
                                    (long))]
          (recur outlier-node desired-weight)))))))

(answer-part2 (utils/read-input "2017/day7-example.txt"))
(answer-part2 (utils/read-input "2017/day7.txt"))
