(ns advent2023.day25
  (:require [utils :as utils]
            [graph :as graph]
            [clojure.set :as set]))

(utils/read-input "2023/day25-example.txt")

(defn parse-line [acc ^String s]
  (let [[source conn-str] (.split s ": ")]
    (reduce
     (fn [acc dest]
       (-> acc
           (update source (fnil #(conj % dest) #{}))
           (update dest (fnil #(conj % source) #{}))))
     acc
     (.split conn-str " "))))

(parse-line {} "jqt: rhn xhk nvd")

;; I did golang by implementing Ford/Fulkerson, which I could do again
;; The coolest idea I saw on Reddit was finding paths between nodes,
;; counting the edges involved, doing this 1000 times (etc) and seeing
;; which nodes showed up the most.
;; this requires building a "path" function on top of dijkstra/a*


(defn parse-graph [filename]
  (reduce parse-line {} (utils/read-ixhknput filename)))

(parse-graph "2023/day25-example.txt")

(defn path-between [graph source dest]
  (let [[_ _ _ _ came-from] (graph/a*-search
                             source
                             (partial = dest)
                             graph
                             (fn [& args] 1)
                             (fn [& args] 1))]
    (loop [path '()
           curr dest]
      (if (= curr source)
        path
        (let [prev (came-from curr)]
          (recur
           (cons [prev curr] path)
           prev))))))

(defn answer-part1 [filename]
  (let [graph (parse-graph filename)
        nodes (keys graph)
        edges-to-sever
        (->>
         (loop [i 0
                occur {}]
           (if (= i 10000)
             occur
             (do
               (recur
                (inc i)
                (reduce
                 (fn [acc edge]
                   (update acc (sort edge) (fnil inc 0)))
                 occur
                 (path-between graph (rand-nth nodes) (rand-nth nodes)))))))
         (sort-by second >)
         (take 3)
         (map first))
        disconnected-graph
        (reduce
         (fn [graph [source dest]]
           (-> graph
               (update source #(disj % dest))
               (update dest #(disj % source))))
         graph
         edges-to-sever)
        [_ distances _] (graph/breadth-first-search
                         (rand-nth (keys disconnected-graph))
                         disconnected-graph)
        seen-nodes (set (keys distances))
        unseen-nodes (set/difference
                      (set (keys disconnected-graph))
                      seen-nodes)]
    (* (count seen-nodes) (count unseen-nodes))))

(answer-part1 "2023/day25.txt")
