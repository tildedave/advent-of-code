(ns advent2015.day19
  (:require [utils :as utils]
            [grid :as grid]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn tokenize [^String chem-string]
  (loop [i 0
         tokens []]
    (if (= i (.length chem-string))
      tokens
      (let [char (.charAt chem-string i)]
        (if (= (inc i) (.length chem-string))
          (conj tokens (str (.charAt chem-string i)))
          (let [next-char (.charAt chem-string (inc i))]
            (if (Character/isLowerCase next-char)
              (recur
               (+ i 2)
               (conj tokens (str char next-char)))
              (recur
               (inc i)
               (conj tokens (str char))))))))))

(defn parse-transforms [lines]
  (->> lines
       (map #(let [[source dest] (.split % " => ")]
                {(tokenize source) [(tokenize dest)]}))
        (reduce (partial merge-with concat) {})))

(defn all-transforms [transforms tokenized-vec]
  ;; looks like we can be stupid as there's no second letter that is also
  ;; the start of a sequence.
  ;; also no first letter that also is a second letter sequence. (whew)
  (->>
   (loop [idx 0
          results []]
     (if-let [x [(get tokenized-vec idx)]]
       (recur
        (inc idx)
        (reduce conj results
                (for [sub (transforms x)]
                  (-> (subvec tokenized-vec 0 idx)
                      (into sub)
                      (into (subvec tokenized-vec (inc idx)))))))
       results
       ))
   (set)))

;; (all-transforms
;;  (parse-transforms (utils/read-input "2015/day19-example.txt"))
;;  (tokenize "HOHOHO"))

(defn answer-part1 [filename]
  (let [[transform-lines [source-str]] (utils/split-by "" (utils/read-input filename))]
    (->> (all-transforms (parse-transforms transform-lines) (tokenize source-str))
         (count))))

;; (answer-part1 "2015/day19.txt")

(defn parse-transforms-p2 [lines]
  (->> lines
       (map #(let [[source dest] (.split % " => ")]
               [dest source]))))

(shuffle (parse-transforms-p2 (utils/read-input "2015/day19-example.txt")))

;; A* search has failed me so we'll do something pretty dumb based on Reddit.
;; https://old.reddit.com/r/adventofcode/comments/3xflz8/comment/cy4cu5b/
;; this is sort of what I was playing around with via reverse map but simpler.
(defn reduce-steps [transforms-p2 molecule-str]
  (loop [curr molecule-str
         n 0]
    (if (= curr "e") n
        (let [[next n]
              (reduce
               (fn [[curr n] [src dest]]
                 (println curr n src dest)
                 (if (.contains curr src)
                   [(.replaceFirst curr src dest) (inc n)]
                   [curr n]))
               [curr n]
               (shuffle transforms-p2))]
          (if (= curr next)
            (recur molecule-str 0)
            (recur next n))))))

;; (defn answer-part2 [transforms dest-string]
;;   (graph/a*-search
;;    "e"
;;    #(= % dest-string)
;;    (partial all-transforms transforms)
;;    (fn [str] (* 100 (- (utils/longest-common-substring dest-string str))))
;;    (fn [curr neighbor] (if (> (count neighbor) (count dest-string)) Integer/MAX_VALUE 1))))

;; (answer-part2  (->> (utils/read-input "2015/day19-example2.txt")
;;                     (parse-transforms)) "HOHOHO")

(println
 (let [[transform-lines [dest-str]] (->> "2015/day19.txt"
                                        (utils/read-input)
                                        (utils/split-by ""))
      transforms (parse-transforms-p2 transform-lines)]
  (reduce-steps transforms dest-str)))

;; (answer-part2  (->> (utils/read-input "2015/day19-example2.txt")
;;                     (parse-transforms)) "HOHOHO")

