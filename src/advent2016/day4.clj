(ns advent2016.day4
  (:require [clojure.string :as string]
            [utils :as utils]))

(defn letter? [ch]
  (Character/isLetter ch))

(defn checksum [room]
  (->> room
       (seq)
       (filter letter?)
       (map #(hash-map % 1))
       (apply merge-with +)
       (sort (fn [[c1 n1] [c2 n2 ]]
               (let [c (compare n1 n2)]
                 (if (= c 0)
                   (compare c1 c2)
                   (- c)))))
       (take 5)
       (map first)
       (string/join)
  ))
    ;;    (map first)
    ;;    (string/join)))

(defn sector-id [room]
  (-> room
      (.split "-")
      (last)
      (utils/parse-int)))

(defn parse-room-with-checksum [line]
  (rest (re-matches #"^([0-9a-z\-]+)\[(\w+)\]" line)))

(parse-room-with-checksum "aaaaa-bbb-z-y-x-123[abxyz]")

(checksum "aaaaa-bbb-z-y-x-123")

(defn is-real-room? [[room-name stated-checksum]]
  (= (checksum room-name) stated-checksum))

(parse-room-with-checksum "not-a-real-room-404[oarel]")
(checksum "not-a-real-room-404")

(defn answer-part1 [lines]
  (->> lines
       (map parse-room-with-checksum)
       (filter is-real-room?)
       (map first)
       (map sector-id)
       (reduce +)))

(answer-part1 ["aaaaa-bbb-z-y-x-123[abxyz]"
               "a-b-c-d-e-f-g-h-987[abcde]"
               "not-a-real-room-404[oarel]"
               "totally-real-room-200[decoy]"])

(answer-part1 (utils/read-input "2016/day4.txt"))
