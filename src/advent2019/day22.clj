(ns advent2019.day22
  (:require [clojure.test :refer [deftest is run-tests testing]]
            [utils :as utils :refer [mod-mult mod-exp mod-inverse]]))

;; OK so I did this before.
;; I believe we need to do part 1 and part 2 separately.
(time (vec (rseq (vec (range 0 1000007)))))

(defn new-deck [size]
  (vec (range 0 size)))

(defn deal-into-new-stack [deck]
  (vec (rseq deck)))

(defn cut-n [deck n]
  (if (< n 0)
    (cut-n deck (+ (count deck) n))
    (into (subvec deck n) (subvec deck 0 n))))

(deftest cut-n-tests
  (is (= (cut-n (new-deck 10) 3) [3 4 5 6 7 8 9 0 1 2]))
  (is (= (cut-n (new-deck 10) -4) [6 7 8 9 0 1 2 3 4 5])))

(defn deal-with-increment [deck n]
  (->> (range 0 (* (count deck) n) n)
       (map-indexed (fn [oldn newn] [(mod newn (count deck)) (deck oldn)]))
       (sort-by first)
       (map second)
       (vec)))

(deal-with-increment (new-deck 10) 3)

(defn apply-instruction [deck instr]
  (if-let [n (second (re-matches #"^deal with increment (\d+)$" instr))]
    (deal-with-increment deck (utils/parse-int n))
    (if (= instr "deal into new stack")
      (deal-into-new-stack deck)
      (if-let [n (second (re-matches #"^cut (-?\d+)$" instr))]
        (cut-n deck (utils/parse-int n))
        (throw (Exception. (format "failed to apply instruction: %s" instr)))))))

(deftest shuffle-tests
  (is
   (= [0 3 6 9 2 5 8 1 4 7]
      (reduce apply-instruction
              (new-deck 10)
              '("deal with increment 7"
                "deal into new stack"
                "deal into new stack"))))
  (is
   (= [3 0 7 4 1 8 5 2 9 6]
      (reduce apply-instruction
              (new-deck 10)
              '("cut 6"
                "deal with increment 7"
                "deal into new stack"))))
  (is
   (= [6 3 0 7 4 1 8 5 2 9]
      (reduce apply-instruction
              (new-deck 10)
              '("deal with increment 7"
                "deal with increment 9"
                "cut -2"))))
  (is
   (= [9 2 5 8 1 4 7 0 3 6]
      (reduce apply-instruction
              (new-deck 10)
              '("deal into new stack"
                "cut -2"
                "deal with increment 7"
                "cut 8"
                "cut -4"
                "deal with increment 7"
                "cut 3"
                "deal with increment 9"
                "deal with increment 3"
                "cut -1")))))

(run-tests 'advent2019.day22)

;; part 1

(->>
 (reduce apply-instruction
         (new-deck 10007)
         (utils/read-input "2019/day22.txt"))
 (map-indexed vector)
 (filter (fn [[_ c]] (= c 2019)))
 (first)
 (first))

;; part 2
;; so we follow the position of our card throughout the entire thing.
;; we can't apply it 101741582076661 times, but it becomes a modular
;; equation that we can directly calculate.

;; deal into new stack = reverse position by num cards - pos - 1
;; cut cards = mod (pos + n) % numCards
;;

(defn reverse-deal-into-new-stack [deck-size pos]
  (- deck-size pos 1))

(defn reverse-cut-n [deck-size pos n]
  (mod (+ n pos) deck-size))

(defn reverse-deal-with-increment [deck-size pos n]
  (mod-mult pos (utils/mod-inverse n deck-size) deck-size))

(defn reverse-instruction [deck-size pos instr]
  (if-let [n (second (re-matches #"^deal with increment (\d+)$" instr))]
    (reverse-deal-with-increment deck-size pos (utils/parse-int n))
    (if (= instr "deal into new stack")
      (reverse-deal-into-new-stack deck-size pos)
      (if-let [n (second (re-matches #"^cut (-?\d+)$" instr))]
        (reverse-cut-n deck-size pos (utils/parse-int n))
        (throw (Exception. (format "failed to apply instruction: %s" instr)))))))

(let [reversed-instructions (reverse (utils/read-input "2019/day22.txt"))
      deck-size 119315717514047
      standard-seq (->>
                    2020
                    (iterate
                     #(reduce (partial reverse-instruction deck-size) % reversed-instructions)))
      [x fx ffx] (take 3 standard-seq)
  ;; so fx = ax + b (deck-size)
  ;; and ffx = afx + b (deck-size)
  ;; a and b are unknowns.  so this is a matrix
  ;; [x  1] [a] = [fx]
  ;; [fx 1] [b] = [ffx]
  ;; a = (fx - ffx) * (x - fx)^1
  ;; b = ffx - a * x
  ;; etc
      a (mod-mult (mod (- fx ffx) deck-size) (utils/mod-inverse (- x fx) deck-size) deck-size)
      b (- fx (mod-mult a x deck-size))
      _ (println x fx ffx)]
;; these agree
;;   (->>
;;    (map vector
;;         standard-seq
;;         (iterate #(mod (+ (mod-mult a % deck-size) b) deck-size) x))
;;    (take 5))
  (defn answer-part2 [n]
    (mod
     (+ (mod-mult (mod-exp a n deck-size) x deck-size)
       (mod-mult
        (dec (mod-exp a n deck-size))
        (mod-mult
         (mod-inverse (dec a) deck-size)
         b
         deck-size)
        deck-size
         ))
     deck-size)))

;; part 2 answer
(answer-part2 101741582076661)
