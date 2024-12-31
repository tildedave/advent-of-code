(ns advent2024.day22
  (:require
    [utils :as utils]))

(defn mix [num1 num2]
  (bit-xor num1 num2))

(assert (= (mix 42 15) 37))

(defn prune [num]
  (bit-and num 0xFFFFFF))

(assert (= (prune 100000000) 16113920))

(defn next-secret [secret]
  (let [secret (prune (mix (bit-shift-left secret 6) secret))
        secret (prune (mix (bit-shift-right secret 5) secret))
        secret (prune (mix (bit-shift-left secret 11) secret))]
    secret))

(next-secret 123)

(defn nth-secret [secret n]
  (nth (iterate next-secret secret) n))

(assert (= 37327623 (reduce + (map #(nth-secret % 2000) '(1 10 100 2024)))))

(reduce + (map #(nth-secret (parse-long %) 2000) (utils/read-input "2024/day22.txt")))

(defn price [secret] (mod secret 10))

(map price (iterate next-secret 123))

(last (take 2001 (iterate next-secret 1)))

;; I wonder if we can brute force this
;; we know certain combinations aren't possible, 9, 9, 9, etc
;; total sum needs to be -9 < x < 9

;; ONLY 45,000 subsequences

(def all-price-drops
  (for [n1 (range -9 10)
        n2 (range -9 10)
        n3 (range -9 10)
        n4 (range -9 10)
        :when (and
               (<= -9 (+ n1 n2) 9)
               (<= -9 (+ n1 n2 n3) 9)
               (<= -9 (+ n1 n2 n3 n4) 9)
               (<= -9 (+ n2 n3 n4) 9)
               (<= -9 (+ n3 n4) 9))]
    [n1 n2 n3 n4]))

(count all-price-drops)

(defn price-seq [secret]
  (take 2001 (map price (iterate next-secret secret))))

(partition 5 1 (price-seq 123))

(partition 5 1 (price-seq 123))

; we are looking 4 ahead I think?
(defn prices-with-drops [secret]
  (->> (partition 5 1 (price-seq secret))
       (map (fn [[a b c d price]] [price [(- b a) (- c b) (- d c) (- price d)]]))))

(defn sequence-benefits [secret]
  (reduce
   (fn [m [price subseq]]
     (cond
       (contains? m subseq) m
       (= price 0) m
       :else (assoc m subseq price)))
   {}
   (prices-with-drops secret)))

(defn answer [secrets]
(->> (map sequence-benefits secrets)
     (reduce (partial merge-with +))
     (sort-by second >)
     (first)
     (second)))

(answer '(1 2 3 2024))
(time (answer (map parse-long (utils/read-input "2024/day22.txt"))))

(count (distinct (take-while #(> (first %) 5) (sort-by first > (prices-with-drops 1)))))

(defn bananas-from [secret subsequence]
  (if-let [first-match
           (first (filter #(= (second %) subsequence) (prices-with-drops secret)))]
    (first first-match)
    0))

(bananas-from 2 [-1 -1 0 2])

(count (map #(bananas-from 2 %) all-price-drops))
