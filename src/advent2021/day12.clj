(ns advent2021.day12
  (:require [advent2021.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as string]))

;; this is just DFS with some restrictions.
;; we can use a bitset to keep track of which visited caverns.

(defn parse-adjacency [lines]
  (->> lines
       (map #(string/split % #"\-"))
       (mapcat (fn [[x s]] [{x #{s}} {s #{x}}]))
       (apply merge-with set/union)))

(defn small-cave? [cave]
  (and (not= cave "start")
       (not= cave "end")
       (Character/isLowerCase (.charAt cave 0))))

(def bitset-idx
  (memoize
   (fn [adj]
     (into {} (->> adj
                   (keys)
                   (map-indexed vector))))))

(defn neighbors [adj [current small-cave-bs path]]
  (let [idx (bitset-idx adj)
        reverse-idx (set/map-invert idx)]
    (remove
     nil?
     (for [neighbor (adj current)]
       (cond
         (= neighbor "end") [neighbor small-cave-bs (cons neighbor path)]
         (= neighbor "start") nil
         (small-cave? neighbor)
         (let [neighbor-idx (reverse-idx neighbor)]
           (if (bit-test small-cave-bs neighbor-idx)
             nil
             [neighbor (bit-set small-cave-bs neighbor-idx) (cons neighbor path)]))
         :else
         [neighbor small-cave-bs (cons neighbor path)])))))

(defn dfs [adj]
  (loop [[queue visited paths] [[["start" 0 '("start")]] #{} #{}]]
    (if (empty? queue)
      paths
      (let [current (first queue)
            [cave _ path] current]
        (if (= cave "end")
          (recur
           [(subvec queue 1)
            visited
            (conj paths path)])
          (recur
           (reduce
            (fn [[queue visited paths] neighbor]
              (if (contains? visited neighbor)
                [queue visited paths]
                [(conj queue neighbor)
                 (conj visited neighbor)
                 paths]))
            [(subvec queue 1) visited paths]
            (neighbors adj current))))))))

(defn answer-part1 [lines]
  (->> lines
       (parse-adjacency)
       (dfs)
       (count)))

(answer-part1 (utils/read-resource-lines "input/day12-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day12-example2.txt"))
(answer-part1 (utils/read-resource-lines "input/day12-example3.txt"))
(answer-part1 (utils/read-resource-lines "input/day12.txt"))
