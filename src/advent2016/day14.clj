(ns advent2016.day14
  (:require [clojure.string :as string]
            [utils :as utils]))

(def md (java.security.MessageDigest/getInstance "MD5"))
(def hex-format (java.util.HexFormat/of))

(defn hash-fn [salt]
  (memoize
   (fn [num]
     (.formatHex hex-format (.digest md (.getBytes (format "%s%d" salt num))))
     )))

(defn stretched-hash [salt]
  (memoize
   (fn [num]
     (->> (iterate #(.formatHex hex-format (.digest md (.getBytes %))) (format "%s%d" salt num))
          (drop 2017)
          (first)))))

((stretched-hash "abc") 0)

(defn triple [s]
  (let [end (dec (count s))]
  (loop [idx 1]
    (cond
      (= idx end) nil
      (= (.charAt s (dec idx)) (.charAt s idx) (.charAt s (inc idx))) (.charAt s idx)
      :else (recur (inc idx))))))

(range -2 2)

(defn five-in-a-row [hash-fn]
  (memoize
   (fn [i]
     (let [s (hash-fn i)
           end (- (count s) 2)]
       (loop [idx 2
              results #{}]
         (cond
           (>= idx end) results
           (apply = (map #(.charAt s %) (range (- idx 2) (+ idx 3))))
           (recur
            (+ idx 3)
            (conj results (.charAt s idx)))
           ;; we can be smarter about stepping forward here actually
           :else (recur (inc idx) results)))))))

(apply = '(\e \e \e \e \e))

(triple ((hash-fn "abc") 18))

(defn is-key? [hash-fn]
  (let [five-in-a-row (five-in-a-row hash-fn)]
    (fn [n]
      (if (zero? (mod n 100)) (println "checking is-key?" n) nil)
      (if-let [triple-ch (triple (hash-fn n))]
        (loop [i (inc n)]
          (cond
            (= i (+ n 1001)) false
            (contains? (five-in-a-row i) triple-ch) true
            :else (recur (inc i))))
        false))))

((five-in-a-row (hash-fn "abc")) 816)

;; (is-key? (hash-fn "abc") 18)
;; (is-key? (hash-fn "abc") 39)

(defn answer [salt]
  (let [hash-fn (hash-fn salt)
        is-key? (is-key? hash-fn)]
    (nth (filter #(is-key? %) (range)) 63)))

;; (answer "abc")
;; (answer (first (utils/read-input "2016/day14.txt")))

(defn answer-part2 [salt]
  (let [hash-fn (stretched-hash salt)
        is-key? (is-key? hash-fn)]
    (nth (filter #(is-key? %) (range)) 63)))

((is-key? (stretched-hash "abc")) 10)

;; (time (println (answer-part2 "abc")))
(time (println (answer-part2 (first (utils/read-input "2016/day14.txt")))))
