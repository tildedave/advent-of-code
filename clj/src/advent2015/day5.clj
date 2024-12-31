(ns advent2015.day5
  (:require [utils :as utils]
            [clojure.set :as set]))

(defn vowel? [ch]
  (case ch
    (\a \e \i \o \u) true
    false))

(defn nice? [str]
  (let [seq-str (seq str)]
    (and (->> seq-str
              (filter vowel?)
              (count)
              (<= 3))
         (->> (map (fn [ch1 ch2] (= ch1 ch2)) seq-str (rest seq-str))
              (filter true?)
              (empty?)
              (not))
         (->> (map (fn [ch1 ch2]
                     (case [ch1 ch2]
                       ([\a \b] [\c \d] [\p \q] [\x \y]) true
                       false)) seq-str (rest seq-str))
              (filter true?)
              (empty?)))))

(nice? "ugknbfddgicrmopn")
(nice? "aaa")
(nice? "jchzalrnumimnmhp")
(nice? "dvszwmarrgswjxmb")

(defn answer-part1 []
  (->> (utils/read-input "2015/day5.txt")
       (filter nice?)
       (count)))

(defn nice-p2? [str]
  (let [seq-str (seq str)
        indexed-seq (map-indexed vector seq-str)]
    (and
     (->>
      (map (fn [[n ch1] [n' ch2]]
             {(format "%c%c" ch1 ch2) #{[n n']}})
           indexed-seq
           (rest indexed-seq))
      (reduce (partial merge-with set/union))
      (filter (fn [[_ s]] (> (count s) 1)))
      (filter (fn [[_ s]] (> (count (flatten (seq s))) 3)))
      (empty?)
      (not))
    (->>
     (map (fn [ch1 ch2 ch3] (= ch1 ch3))
          seq-str
          (rest seq-str)
          (rest (rest seq-str))
          )
     (filter true?)
     (empty?)
     (not)
     ))))


(defn answer-part2 []
  (->> (utils/read-input "2015/day5.txt")
       (filter nice-p2?)
       (count)))

(answer-part2)

(let [indexed-seq (seq "uurcxstgmygtbstg")]
  (map (fn [ch1 ch2 ch3] (= ch1 ch3))
       indexed-seq
       (rest indexed-seq)
       (rest (rest indexed-seq))))


(flatten (seq #{[10 11] [0 1]}))

    )
  )

(answer-part1)
(let [seq-str (seq "jchzalrnumimnmhp")]
  (and)(->> (map (fn [ch1 ch2] (= ch1 ch2)) seq-str (rest seq-str))
       (filter true?)))

(let [seq-str (seq "jchzalrnumimnmhp")]
  (map (fn [ch1 ch2] (println ch1 ch2) (= ch1 ch2)) seq-str (rest seq-str)))
