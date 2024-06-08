(ns advent2018.day20
  (:require [advent2020.day18 :refer [matching-paren]]))

;; so we use the regex to construct the map
;; then we run dijskstra to find the distances
;; then we find the longest one.
(.indexOf "abc" "d")

(.substring "abc" 1)

;; parsing the regex seems frustrating

(defn parse-door-regex [^String s]
  (println "parse-door-regex" s)
  (if (.startsWith s "^")
    (parse-door-regex (.substring s 1 (dec (count s))))
    (loop [s s
           result []]
      (let [lparen-idx (.indexOf s "(")
            bar-idx (.indexOf s "|")
            _ (println s lparen-idx bar-idx)]
        (cond
          (= lparen-idx bar-idx -1) (if (empty? s)
                                      result
                                      (conj result s))
         ;; so the next thing to process is a (
          (or (= bar-idx -1) (< -1 lparen-idx bar-idx))
          (let [rparen-idx (matching-paren s lparen-idx)]
            (recur
             (.substring s (inc rparen-idx))
             (conj result
                   [:concat
                    (.substring s 0 lparen-idx)
                    (parse-door-regex (.substring s (inc lparen-idx) rparen-idx))])))
          (or (= lparen-idx -1) (< -1 bar-idx lparen-idx))
          (let [next-str (.substring s (inc bar-idx))
                _ (println "next thing is a bar" next-str)]
            (if (empty? next-str)
              (recur
               next-str
               (-> result
                   (conj (.substring s 0 bar-idx))
                   (conj "")))
              (recur
               next-str
               (conj result (.substring s 0 bar-idx)))))
          :else (throw (Exception.  "should never happen")))))))

(defn step-char [[[cx cy] door-map] ch]
  (let [[dx dy] (case ch
                  \N [0 -1]
                  \E [1 0]
                  \W [-1 0]
                  \S [0 1])]
    [[(+ cx dx) (+ cy dy)] (update door-map [cx cy] (fnil #(conj % ch) []))]))

(defn step-regex [state item]
  (cond
    (string? item) (reduce step-char state (seq item))
    (and (vector? item) (= (first item) :concat))
    (let [[_ first-s then-l] item]
    ;; we do the first (updating the coords), then we recurse on the second.
      (reduce
       step-regex
       (reduce step-char state (seq first-s))
       then-l))
    ;; otherwise we have a list of options from the current position.
    (vector? item)
    (reduce
     (fn [[[cx cy] door-map] item]
       (let [[_ door-map] (step-regex [[cx cy] door-map] item)]
         [[cx cy] door-map]))
     state
     item)
    :else
        (throw (Exception. "should be impossible"))))

(reduce step-char [[0 0] {}] (seq "NENENENENE"))
(step-regex [[0 0] {}] (parse-door-regex "^ENWWW(NEEE|SSE(EE|N))$"))

(parse-door-regex "^NENENENENE")

(parse-door-regex "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
