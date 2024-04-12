(ns advent2015.day8
  (:require [clojure.string :as string]
            [utils :as utils]))

(defn strip-hex-codes [str]
  (loop [str str]
    (if-let [[_ first code rest] (re-matches #"(.*)\\x([0-9a-f]{2})(.*)" str)]
      (recur
       (string/join
        [first
         (Character/toString (char (read-string "0x62")))
         rest]))
      str)))

(strip-hex-codes "\"irde\\x85\\x5cvbah\\jekw\"ia\"")

(.replaceAll "\"aaa\"aaa\"" "\"" "a")

(defn in-memory-length [str]
  (-> str
      (.substring 1 (dec (.length str)))
      (strip-hex-codes)
      (string/replace #"\\\"" "q")
      (string/replace #"\\\\" "q")
      (.length)
      ))

(->> (utils/read-input "2015/day8-example.txt")
     (map in-memory-length))

(defn in-memory-diff [str]
  (- (count str) (in-memory-length str)))

(defn answer-part1 [filename]
  (->> filename
       (utils/read-input)
       (map in-memory-diff)
       (reduce +)))

(answer-part1 "2015/day8-example.txt")
(answer-part1 "2015/day8.txt")
