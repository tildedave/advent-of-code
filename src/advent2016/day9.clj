(ns advent2016.day9
  (:require [utils :as utils]))

(def ^:dynamic part2? false)

(defn decompressed-length [s]
  (loop [idx 0
         result 0]
     (cond
       (= idx (.length s)) result
       (= (.charAt s idx) \()
        ;; walk forward until we find the \) and grab the marker
       (let [end (.indexOf s ")" idx)
             marker (.substring s idx (inc end))
             [num-chars n] (->> marker
                                (re-matches #"\((\d+)x(\d+)\)")
                                (rest)
                                (map utils/parse-int))
             repeat-start (inc end)
             repeat-end (+ (inc end) num-chars)
             str-to-repeat (.substring s repeat-start repeat-end)]
         (recur repeat-end
                (+ result (* n (if part2?
                                 (decompressed-length str-to-repeat)
                                 (- repeat-end repeat-start))))))
       :else (recur (inc idx) (+ result 1)))))

(defn answer-part1 []
  (->> (utils/read-input "2016/day9.txt")
       (first)
       (decompressed-length)))

(answer-part1)

(defn answer-part2 []
  (binding [part2? true]
    (->> (utils/read-input "2016/day9.txt")
       (first)
       (decompressed-length))))

(answer-part2)
