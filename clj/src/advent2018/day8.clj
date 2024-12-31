(ns advent2018.day8
  (:require [utils :as utils]))

(defn parse-tree [num-seq]
  (let [[num-children num-metadata] num-seq
        [final-num-seq children] (reduce
                                  (fn [[num-seq children] _]
                                    (let [[tree rest-num-seq] (parse-tree num-seq)]
                                      [rest-num-seq (conj children tree)]))
                                  [(drop 2 num-seq) []]
                                  (range num-children))]
    [{:children children
      :metadata (take num-metadata final-num-seq)}
     (drop num-metadata final-num-seq)]))

(defn metadata-seq [{:keys [children metadata]}]
  (lazy-seq (cons metadata (map metadata-seq children))))

(defn answer-part1 [num-seq]
  (let [[tree _] (parse-tree num-seq)]
    (reduce + (flatten (metadata-seq tree)))))

(answer-part1 '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

(defn parse-file [filename]
  (map utils/parse-int
       (.split (first (utils/read-input filename)) "\\s+")))

(answer-part1 (parse-file "2018/day8.txt"))

(defn tree-value [{:keys [children metadata]}]
  (if (empty? children)
    (reduce + metadata)
    ;; otherwise we recur over the children based on the metadata entries I guess?
    (reduce + (map #(let [child (first (drop (dec %) children))]
                      (if child (tree-value child) 0))
                   metadata))))

(defn answer-part2 [num-seq]
  (let [[tree _] (parse-tree num-seq)]
    (tree-value tree)))

(answer-part2 '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))
(answer-part2 (parse-file "2018/day8.txt"))
