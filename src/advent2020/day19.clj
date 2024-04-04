(ns advent2020.day19
  (:require [utils :as utils]
            [clojure.string :as string]))

(defn parse-rule [rule-line]
  (let [[rule-num-str rest-line] (.split rule-line ": ")
        rule-num (utils/parse-int rule-num-str)]
    {rule-num
     (if-let [[_ ch] (re-matches #"^\"(a|b)\"$" rest-line)]
      (.charAt ch 0)
      (->> (string/split rest-line #" \| ")
           (map #(.split % " "))
           (map #(mapv utils/parse-int %))))}))


(.split "1 2" " | ")

(parse-rule "0: \"a\"")
(parse-rule "0: 1 2 | 3 4")
(parse-rule "0: 1 2")

(defn match-prefix [rules rule-num line-seq]
;;   (println "match-prefix" rule-num line-seq)
  (let [current-rule (rules rule-num)]
    (if (or (= current-rule \a) (= current-rule \b))
      (if (= (first line-seq) current-rule)
        (list (rest line-seq))
        nil)
      ;; the current rule is some combination of prefix/suffixes
      ;; for each one, we may generate a match.  thus our result
      ;; is a list of matches
      (apply concat
             (for [rule-num current-rule]
               (loop [prefix-list rule-num
                      remaining [line-seq]]
                 (if-let [first-prefix (first prefix-list)]
                   (recur
                    (rest prefix-list)
                    (->> remaining
                         (map #(match-prefix rules first-prefix %))
                         (remove nil?)
                         (apply concat)))
                   remaining)))))))

(defn parse-input [filename]
  (let [[rule-lines lines] (->> filename
                                (format "2020/%s")
                                (utils/read-input)
                                (utils/split-by ""))]
    {:rules (->> rule-lines (map parse-rule) (reduce merge))
     :lines lines}))

(parse-input "day19-full-example.txt")

(defn answer-part1 [filename]
  (let [{:keys [lines rules]} (parse-input filename)]
    (->> lines
         (filter (partial matches? rules))
         (count))))

(answer-part1 "day19-full-example.txt")

(defn matches? [rules str]
  (let [results (match-prefix rules 0 (seq str))]
    (and (not (empty? results))
         (every? empty? results))))

(match-prefix (answer-part1 "day19-example2.txt") 0 (seq "aaaabbb"))

(assert (matches? (answer-part1 "day19-example.txt") "aba"))
(assert (not (matches? (answer-part1 "day19-example2.txt") "aaaabbb")))


(match-prefix
 (answer-part1 "day19-example.txt")
 0
 (seq "aba"))

(->> (string/split (parse-rule "0: 1 2") #" \| ")
     (map string/split )



