(ns advent2018.day20
  (:require [advent2020.day18 :refer [matching-paren]]
            [graph :as graph]
            [utils :as utils]))

;; so we use the regex to construct the map
;; then we run dijskstra to find the distances
;; then we find the longest one.
(.indexOf "abc" "d")

(.substring "abc" 1)

;; parsing the regex seems frustrating
;; this is so buggy.

(defn to-next-bar
  "Given a string, chop off the current 'or' clause from it."
  [^String s]
  (let [lparen-idx (.indexOf s "(")
        bar-idx (.indexOf s "|")]
    (cond
      (= bar-idx -1) [s "" false]
      (or (= lparen-idx -1) (< -1 bar-idx lparen-idx)) [(.substring s 0 bar-idx)
                                                        (.substring s (inc bar-idx))
                                                        (> bar-idx -1)]
      :else ;; lparen is first, we need to recur
      (let [rparen-idx (matching-paren s lparen-idx)
            [next-bar rest-s has-bar?] (to-next-bar (.substring s (inc rparen-idx)))]
        [(str (.substring s 0 (inc rparen-idx))
              next-bar)
         rest-s
         has-bar?]))))

(to-next-bar "W")

(declare parse-clause parse-door-regex)

(defn parse-clause [^String s]
  ;; we know there are no toplevel |s in this, so we are happy.
  (let [lparen-idx (.indexOf s "(")]
    (if
     (= lparen-idx -1) s
     (let [rparen-idx (matching-paren s lparen-idx)]
       [:concat
        [:concat
         (parse-door-regex (.substring s 0 lparen-idx))
         (parse-door-regex (.substring s (inc lparen-idx) rparen-idx))]
        (parse-clause (.substring s (inc rparen-idx)))]))))

(to-next-bar "W|")

(parse-door-regex "^(N|S|W|)(A|B|C)$")

(defn parse-door-regex [^String s]
  (if (.startsWith s "^")
    (parse-door-regex (.substring s 1 (dec (count s))))
    (loop [s s
           or-clause []]
      (if (empty? s)
        or-clause
        (let [[clause rest-clause has-next-bar?] (to-next-bar s)]
          (if (and (empty? rest-clause) has-next-bar?)
            (-> or-clause
                (conj (parse-clause clause))
                (conj [""]))
            (recur rest-clause (conj or-clause (parse-clause clause)))))))))

;;   (loop [s s
;;            ;; result is the or-clause
;;          or-clause []]
;;     (let [lparen-idx (.indexOf s "(")
;;           bar-idx (.indexOf s "|")]
;;       (cond
;;         (= lparen-idx bar-idx -1) (if (empty? s)
;;                                     or-clause
;;                                     (conj or-clause s))
;;           ;; so the next thing to process is a (
;;           ;; based on how the recurring is set up,
;;           ;; we need to process the "full clause" (to the next |)
;;           ;; before recurring.
;;           ;; I think what we have to do is find the next bar,
;;           ;; if a paren is there, glom,
;;         (= bar-idx -1) (conj or-clause (parse-door-regex s))
;;         (< -1 lparen-idx bar-idx)
;;         (loop)
;;         (let [rparen-idx (matching-paren s lparen-idx)]
;;           [:concat
;;            [:concat
;;             (.substring s 0 lparen-idx)
;;             (parse-door-regex (.substring s (inc lparen-idx) rparen-idx))]
;;            (parse-door-regex (.substring s (inc rparen-idx)))])
;;         (or (= lparen-idx -1) (< -1 bar-idx lparen-idx))
;;         (let [next-str (.substring s (inc bar-idx))]
;;           (if (empty? next-str)
;;             (recur
;;              next-str
;;              (-> or-clause
;;                  (conj (.substring s 0 bar-idx))
;;                  (conj "")))
;;             (recur
;;              next-str
;;              (conj or-clause (.substring s 0 bar-idx)))))
;;         :else (throw (Exception.  "should never happen")))))))

(parse-door-regex "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")

(defn step-char [[[cx cy] door-map] ch]
  (let [[dx dy] (case ch
                  \N [0 -1]
                  \E [1 0]
                  \W [-1 0]
                  \S [0 1])
        [nx ny] [(+ cx dx) (+ cy dy)]]
    [[nx ny]
     (update door-map [cx cy] (fnil #(conj % [nx ny]) #{}))]))

(defn step-regex [state item]
  (cond
    (string? item) (reduce step-char state (seq item))
    (and (vector? item) (= (first item) :concat))
    (let [[_ first-re then-l] item]
    ;; we do the first (updating the coords), then we recurse on the second.
      (step-regex
       (step-regex state first-re)
       then-l))
    ;; otherwise we have a list of options from the current position.
    (vector? item)
    (if (= (count item) 1)
      (step-regex state (first item))
      (reduce
       (fn [[[cx cy] door-map] item]
         (let [[_ door-map] (step-regex [[cx cy] door-map] item)]
           [[cx cy] door-map]))
       state
       item))
    :else
    (throw (Exception. "should be impossible"))))

(defn neighbors [door-map]
  (fn [[x y]]
    (get door-map [x y] #{})))

(defn answer [regex]
  (let [[_ door-map] (step-regex [[0 0] {}] (parse-door-regex regex))]
    (->> (graph/dijkstra-search
          [0 0]
          (neighbors door-map)
          (fn [& args] false))
         (vals)
         (reduce max))))

(step-regex [[0 0] {}] (parse-door-regex "^ENWWW(NEEE|SSE(EE|N))$"))
(step-regex [[0 0] {}] (parse-door-regex "^ENWWW$"))

(assert (= 3 (answer "^WNE$")))
(assert (= 10 (answer "^ENWWW(NEEE|SSE(EE|N))$")))
(assert (= 18 (answer "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")))
(assert (= 23 (answer "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")))
(assert (= 31 (answer "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")))

(->> (utils/read-input "2018/day20.txt") (first) (answer))

(defn answer-part2 [regex]
  (let [[_ door-map] (step-regex [[0 0] {}] (parse-door-regex regex))]
    (->> (graph/dijkstra-search
          [0 0]
          (neighbors door-map)
          (fn [& args] false))
         (vals)
         (filter (partial <= 1000))
         (count))))

(->> (utils/read-input "2018/day20.txt") (first) (answer-part2))
