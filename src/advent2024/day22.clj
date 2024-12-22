(ns advent2024.day22)

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

(reduce + (map #(nth-secret % 2000) '(1 10 100 1024)))

(first (drop 2000 (iterate next-secret 1)))
