(ns advent2024.day13
  (:require [clojure.math :as math]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as ml]
            [utils :as utils]))

;; good ol'fashioned linear algebra

(def q [[94 22] [34 67]])

;; (ml/solve q [8400 5400])

;; (ml/solve [[26 67] [66 21]] [12748 12176])

(def ^:dynamic part2? false)

(m/det [[26 67] [66 21]])
(m/det [[94 22] [34 67]])

(utils/str->nums "Button A: X+94, Y+34")

(defn parse-buttons [[line1 line2 line3]]
  (let [[x1 y1] (utils/str->nums line1)
        [x2 y2] (utils/str->nums line2)
        [a b] (utils/str->nums line3)]
    [[x1 x2 (if part2? (+ a 10000000000000) a)]
     [y1 y2 (if part2? (+ b 10000000000000) b)]]))

(defn solve-buttons [[[x1 x2 x] [y1 y2 y]]]
  (let [double-result (ml/solve (m/matrix :vectorz [[x1 x2] [y1 y2]]) [x y])
        [t1 t2] (map #(math/round %) double-result)]
    (if
     (and
      (= a (+ (* x1 t1) (* x2 t2)))
      (= b (+ (* y1 t1) (* y2 t2)))
      (<= t1 100)
      (<= t2 100))
      (+ (* 3 t1) t2)
      0)))

(def example
  '("Button A: X+94, Y+34"
    "Button B: X+22, Y+67"
    "Prize: X=8400, Y=5400"
    ""
    "Button A: X+26, Y+66"
    "Button B: X+67, Y+21"
    "Prize: X=12748, Y=12176"
    ""
    "Button A: X+17, Y+86"
    "Button B: X+84, Y+37"
    "Prize: X=7870, Y=6450"
    ""
    "Button A: X+69, Y+23"
    "Button B: X+27, Y+71"
    "Prize: X=18641, Y=10279"))

(map parse-buttons (utils/split-by "" example))

(parse-buttons '("Button A: X+26, Y+66"
                 "Button B: X+67, Y+21"
                 "Prize: X=10000000012748, Y=10000000012176"))

(parse-buttons '("Button A: X+94, Y+34"
                 "Button B: X+22, Y+67"
                 "Prize: X=8400, Y=5400"))



(parse-buttons '("Button A: X+26, Y+66"
                 "Button B: X+67, Y+21"
                 "Prize: X=12748, Y=12176"))

(->> (utils/read-input "2024/day13.txt")
     (utils/split-by "")
     (map parse-buttons)
     (map solve-buttons)
     (reduce +))

;; OK, we can find solutions mod p (for some number of ps) then use CRT to
;; find the answer.  how many p's do we need?
;; (also it could be that our residues compute an answer that doesn't work,
;; still gotta check that)

(def first-n-primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67))  ;;71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271))
(defn system-mod-p [p [[x1 x2 a] [y1 y2 b]]]
  [[(mod x1 p) (mod x2 p) (mod a p)]
   [(mod y1 p) (mod y2 p) (mod b p)]])


(m/mmul [[1 2] [3 4]] [[1] [2]])
(defn solve-mod-p [p [[a b x] [c d y]]]
  (let [det (mod (- (* a d) (* b c)) p)]
    (if (zero? det)
      (throw (ArithmeticException. "singular matrix; no solution"))
      (let [inv-det (utils/mod-inverse det p)
            inv-mod-p (m/mul
                       [[(mod d p) (mod (- b) p)]
                        [(mod (- c) p) (mod a p)]]
                       inv-det)
            [t1 t2] (m/mmul inv-mod-p [(mod x p) (mod y p)])]
        [(int t1) (int t2)]))))

(defn residues [button-system]
    (->> first-n-primes
         (map #(let [[t1' t2'] (try
                                 (solve-mod-p % button-system)
                                 (catch ArithmeticException _ [nil nil]))]
                 [[t1' %] [t2' %]]))
         (remove #(or (nil? (first (first %)))
                      (nil? (first (second %)))))))

(residues
 (parse-buttons '("Button A: X+26, Y+66"
                  "Button B: X+67, Y+21"
                  "Prize: X=10000000012748, Y=10000000012176")))

(defn num-tokens-p2 [button-system]
  (let  [[[x1 x2 x] [y1 y2 y]] button-system
         residues (residues button-system)]
    (loop [n 1]
      (let [t1-real (utils/crt-inductive (map first (take n residues)))
            t2-real (utils/crt-inductive (map second (take n residues)))
            x-real (+ (* x1 t1-real) (* x2 t2-real))
            y-real (+ (* y1 t1-real) (* y2 t2-real))]
        (cond
          (and (= x-real x) (= y-real y)) (+ (* 3 t1-real) t2-real)
          (or (> x-real x) (> y-real y)) 0
          :else (recur (inc n)))))))

(binding [part2? true]
  (->> (utils/read-input "2024/day13.txt")
     (utils/split-by "")
     (map parse-buttons)
     (map num-tokens-p2)
     (reduce +)))

;; 88998053895996 is too low

(num-tokens-p2)
;; (defn residues [[x1 y1 a b] p]
;;   [(mod x1 p) (mod a p)]
;;   [(mod y1 p) (mod b p)])

;; must solve residues separately

               (utils/crt-inductive
                [[]])
