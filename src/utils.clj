(ns utils
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(defn read-resource-lines [resource]
  (line-seq (io/reader (io/resource resource))))

(def read-input read-resource-lines)

(defn parse-int [str] (Integer/valueOf str))

(defn quot-round-up [n m]
  (case (mod n m)
    0 (quot n m)
    (inc (quot n m))))

(defn try-parse-int [str]
  (try
    (Integer/valueOf str)
    (catch IllegalArgumentException _ str)))

(defn xor [b1 b2]
  (case [b1 b2]
    [true false] true
    [false true] true
    false))

(defn split-by [s ^String lines]
  (->> lines
       (partition-by (partial = s))
       (remove (partial = (list s)))))

(defn sml-partition
  "SML's List.partition, which I find myself reaching for."
  ([f s] (sml-partition f s (list) (list)))
  ([f s true-list false-list]
   (loop [s s
          true-list true-list
          false-list false-list]
     (if-let [x (first s)]
       (case (f x)
         true (recur (rest s)
                     (conj true-list x)
                     false-list)
         false (recur (rest s)
                      true-list
                      (conj false-list x)))
       [true-list false-list]))))

;; https://github.com/clojure/core.incubator/blob/be509fd967df8ce1ee43c43bca52360cf710252a/src/main/clojure/clojure/core/incubator.clj#L63-L75
(defn dissoc-in
  " Dissociates an entry from a nested associative structure returning a new
nested structure. keys is a sequence of keys. Any empty maps that result
will not be present in the new structure. "
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

;; https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Longest_common_substring#Clojure
(defn longest-common-substring
  [str1 str2]
  (loop [s1 (seq str1), s2 (seq str2), len 0, maxlen 0]
    (cond
      (>= maxlen (count s1)) maxlen
      (>= maxlen (+ (count s2) len)) (recur (rest s1) (seq str2) 0 maxlen)
      :else (let [a (nth s1 len ""), [b & s2] s2, len (inc len)]
              (if (= a b)
                (recur s1 s2 len (if (> len maxlen) len maxlen))
                (recur s1 s2 0 maxlen))))))


(defn combinations-up-to [coll n]
  (apply concat
         (for [i (range (inc n))] (combo/combinations coll i))))

(combinations-up-to [1 2 3 4 5 6] 2)

;; Knuth Algo X 4.5.2
(defn euclid-extended [u v]
  (loop
   [uv [1 0 u] vv [0 1 v]]
    (let [[_ _ u3] uv
          [_ _ v3] vv]
      (if (zero? v3) uv
        (let [q (quot u3 v3)]
          (recur
           vv
           (mapv - uv (mapv (partial * q) vv))))))))

;; 1.3.12 Course in Computational Algebraic Number Theory
(defn crt-inductive [residues]
  (loop [[x m] (first residues)
         residues (rest residues)]
    (if (empty? residues) x
        (let [[x' m'] (first residues)
              [u v d] (euclid-extended m m')
              ;; *' "auto-promotes" to bigint
              [x m] [(+ (*' u m x') (*' v m' x)) (* m m')]]
          (assert (= d 1))
          (recur [(mod x m) m] (rest residues))))))

(assert (= (crt-inductive '([0 3] [3 4] [4 5])) 39))

(crt-inductive '([0 5] [1 2]))

(let [md (java.security.MessageDigest/getInstance "MD5")
      hex-format (java.util.HexFormat/of)]
  (defn md5-hex [^String s]
    (->> s
         (.getBytes)
         (.digest md)
         (.formatHex hex-format))))

; https://stackoverflow.com/a/8642069/576087
(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))


(defn mean [nums]
  (/ (reduce + 0.0 nums) (count nums)))

(mean '(1 2 10))

(defn manhattan-distance [coord1 coord2]
  (->> (map - coord1 coord2)
       (map abs)
       (reduce +)))

(manhattan-distance [0 0 0] [0 2 0])

(defn range-inclusive [m n]
  (range m (inc n)))

(defn to-digits [n left]
  (if (zero? left)
    []
    (conj (to-digits (quot n 10) (dec left)) (mod n 10))))
