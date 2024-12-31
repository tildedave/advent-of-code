(ns advent2021.day12
  (:require [utils :as utils]
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

(def ^:dynamic part2? false)

(defn neighbors [adj [current small-cave-bs single-cave path]]
  (let [idx (bitset-idx adj)
        reverse-idx (set/map-invert idx)]
    (remove
     nil?
     (for [neighbor (adj current)]
       (cond
         (= neighbor "end") [neighbor small-cave-bs single-cave (cons neighbor path)]
         (= neighbor "start") nil
         (small-cave? neighbor)
         (let [neighbor-idx (reverse-idx neighbor)]
           (if (bit-test small-cave-bs neighbor-idx)
             (if (and (nil? single-cave) part2?)
               [neighbor small-cave-bs neighbor (cons neighbor path)]
               nil)
             [neighbor (bit-set small-cave-bs neighbor-idx) single-cave (cons neighbor path)]))
         :else
         [neighbor small-cave-bs single-cave (cons neighbor path)])))))

(defn dfs [adj]
  (loop [[queue visited paths] [[["start" 0 nil '("start")]] #{} #{}]]
    (if (empty? queue)
      paths
      (let [current (first queue)
            [cave _ _ path] current]
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


(defn answer-part2 [lines]
  ;; I hope this doesn't awaken something in me
  (binding [part2? true]
    (->> lines
       (parse-adjacency)
       (dfs)
       (count))))

(answer-part2 (utils/read-resource-lines "input/day12-example.txt"))
(answer-part2 (utils/read-resource-lines "input/day12-example2.txt"))
(answer-part2 (utils/read-resource-lines "input/day12-example3.txt"))
;; this is slow but whatever.
(answer-part2 (utils/read-resource-lines "input/day12.txt"))
