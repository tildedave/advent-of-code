(ns advent2021.day16
  (:require [utils :as utils]))

(def hex-map
  {\0 '(0 0 0 0)
   \1 '(0 0 0 1)
   \2 '(0 0 1 0)
   \3 '(0 0 1 1)
   \4 '(0 1 0 0)
   \5 '(0 1 0 1)
   \6 '(0 1 1 0)
   \7 '(0 1 1 1)
   \8 '(1 0 0 0)
   \9 '(1 0 0 1)
   \A '(1 0 1 0)
   \B '(1 0 1 1)
   \C '(1 1 0 0)
   \D '(1 1 0 1)
   \E '(1 1 1 0)
   \F '(1 1 1 1)})

(mapcat hex-map "D2FE28")

(defn bits-to-num [binary-seq]
  (Long/parseLong (apply str binary-seq) 2))

(defn parse-value [bit-seq]
  (loop [seq bit-seq
         bits []]
    (case (first seq)
      0 [(bits-to-num (apply concat (conj bits (take 4 (rest seq)))))
         (drop 5 seq)]
      1 (recur (drop 5 seq)
               (conj bits (take 4 (rest seq)))))))

;; forward decl so I can mutually recurse
(declare parse-bit-seq)

(defn parse-operator [bit-seq]
  (let [length-type (first bit-seq)]
    (case length-type
      0 (let [length (bits-to-num (take 15 (rest bit-seq)))
              bit-seq (drop 16 bit-seq)]
          ;; we parse length bits, and drop length bits.
          [(loop [bit-seq (take length bit-seq)
                  result []]
             (if (empty? bit-seq)
               result
               (let [[packet remaining] (parse-bit-seq bit-seq)]
                 (recur remaining (conj result packet)))))
           (drop length bit-seq)])
      1 (let [num-packets (bits-to-num (take 11 (rest bit-seq)))
              bit-seq (drop 12 bit-seq)]
          (loop [bit-seq bit-seq
                 remaining num-packets
                 result []]
            (if (= remaining 0) [result bit-seq]
                (let [[packet remaining-seq] (parse-bit-seq bit-seq)]
                  (recur remaining-seq (dec remaining) (conj result packet)))))))))

(defn parse-bit-seq [bit-seq]
  (let
   [header {:version (->> bit-seq (take 3) (bits-to-num))
            :type (->> bit-seq (drop 3) (take 3) (bits-to-num))}
    bit-seq (drop 6 bit-seq)
    [contents remaining] (case (header :type)
                           4 (parse-value bit-seq)
                           (parse-operator bit-seq))]
               ;; operator packet

            ;;        )
            ;;      ))
            ;;    )]
    [(assoc header :contents  contents) remaining]))


(defn parse-packet [hex-str]
  (->> hex-str
       (mapcat hex-map)
       (parse-bit-seq)
       (first)))

(parse-packet "D2FE28")
(parse-packet "38006F45291200")
(parse-packet "EE00D40C823060")


(defn version-nums [packet]
  (let [{:keys [version contents]} packet]
    (cons version
          (if (number? contents) nil
              (lazy-seq (mapcat version-nums contents))))))

(defn answer-part1 [hex-str]
  (->> hex-str
       (parse-packet)
       (version-nums)
       (reduce +)))

;; EZ
(answer-part1 "8A004A801A8002F478")
(answer-part1 "620080001611562C8802118E34")
(answer-part1 "C0015000016115A2E0802F182340")
(answer-part1 "A0016C880162017C3686B18A3D4780")
(answer-part1 (first (utils/read-input "day16.txt")))

(defn eval-packet [packet]
  (let [{:keys [type contents]} packet]
    (case type
      4 contents
      0 (reduce + (map eval-packet contents))
      1 (reduce * (map eval-packet contents))
      2 (reduce min (map eval-packet contents))
      3 (reduce max (map eval-packet contents))
      5 (if (apply > (map eval-packet contents)) 1 0)
      6 (if (apply < (map eval-packet contents)) 1 0)
      7 (if (apply = (map eval-packet contents)) 1 0))))

(defn answer-part2 [hex-str]
  (->> hex-str
       (parse-packet)
       (eval-packet)))

(assert (= 3 (answer-part2 "C200B40A82")))
(assert (= 54 (answer-part2 "04005AC33890")))
(assert (= 7 (answer-part2 "880086C3E88112")))
(assert (= 9 (answer-part2 "CE00C43D881120")))
(assert (= 1 (answer-part2 "D8005AC2A8F0")))
(assert (= 0 (answer-part2 "F600BC2D8F")))
(assert (= 0 (answer-part2 "9C005AC2F8F0")))
(assert (= 1 (answer-part2 "9C0141080250320F1802104A08")))

(answer-part2 (first (utils/read-input "day16.txt")))
