(ns advent2016.day9
  (:require [clojure.string :as string]
            [utils :as utils]))

(.indexOf "abc" "c" 1)

(defn decompress-string [^String s]
  (loop [idx 0
         result []]
    (cond
      (= idx (.length s)) (string/join result)
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
               (conj result (string/join (repeat n str-to-repeat)))))
      :else (recur (inc idx) (conj result (str (.charAt s idx)))))))

(decompress-string "A(1x5)BC")
(decompress-string "(3x3)XYZ")
(decompress-string "A(2x2)BCD(2x2)EFG")
(decompress-string "(6x1)(1x3)A")
(decompress-string "X(8x2)(3x3)ABCY")

(defn answer-part1 []
  (->> (utils/read-input "2016/day9.txt")
       (first)
       (decompress-string)
       (count)))
(answer-part1)

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
                (+ result (* n (decompressed-length str-to-repeat)))))
       :else (recur (inc idx) (+ result 1)))))

(defn answer-part2 []
  (->> (utils/read-input "2016/day9.txt")
       (first)
       (decompressed-length)))

(answer-part2)
