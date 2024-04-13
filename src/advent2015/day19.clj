(ns advent2015.day19
  (:require [utils :as utils]
            [clojure.string :as string]))

(defn parse-transforms [lines]
  (->> lines
       (map #(let [[source dest] (.split % " => ")]
                {source [dest]}))
        (reduce (partial merge-with concat) {})))

(defn all-transforms [transforms ^String source-string]
  ;; looks like we can be stupid as there's no second letter that is also
  ;; the start of a sequence.
  ;; also no first letter that also is a second letter sequence. (whew)
  (->>
   (loop [^String suffix source-string
         prefix (StringBuilder.)
         results []]
    (if
      (.isEmpty suffix) results
      (let [first-str (.substring suffix 0 1)
            rest-str (.substring suffix 1 (.length suffix))]
        (if (contains? transforms first-str)
          (let [prefix-str (.toString prefix)]
            (recur
             rest-str
           ;; recursing assumes NO further transforms
             (.append prefix first-str)
             (reduce conj results
                     (for [sub (transforms first-str)]
                       (string/join [prefix-str sub rest-str])))))
          (if (>= (.length suffix) 2)
            (let [next-two-str (.substring suffix 0 2)
                  rest-two-str (.substring suffix 2 (.length suffix))]
              (if (contains? transforms next-two-str)
                (let [prefix-str (.toString prefix)]
                  (recur
                   rest-two-str
                   (.append prefix next-two-str)
                   (reduce conj results
                           (for [sub (transforms next-two-str)]
                             (string/join [prefix-str sub rest-two-str])))))
                (recur
                 rest-str
                 (.append prefix first-str)
                 results))))))
                  ])

   (set)))

(all-transforms
 (->> (utils/read-input "2015/day19-example.txt")
     (parse-transforms))
 "HOHOHO")

;;                    )
;;       (contains? transforms seq-1)
;;       (recur
;;        (drop 1 str-seq)
;;        (.append prefix (string/join seq-1))
;;        (reduce conj results
;;                (.app)
;;                (transforms seq-1)
;;        ;; add all the transforms here.
;;        )
;;       (and (= (count seq-2 2) (contains? transforms seq-2)))

;;         (let [x (first str-seq)])
;;         )
;;     )
;;   )
