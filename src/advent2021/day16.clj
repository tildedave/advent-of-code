(ns advent2021.day16
  (:require [advent2021.utils :as utils]))

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
