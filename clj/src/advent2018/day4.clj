(ns advent2018.day4
  (:require [utils :as utils]))

(def formatter (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))

(def parse-log-line #(let [[_ timestamp r] (re-matches #"\[([^]]+)\] (.*)$" %)]
                       [(java.time.LocalDateTime/parse timestamp formatter) r]))

(defn asleep-minutes [acc [^java.time.LocalDateTime timestamp line]]
  (if-let [[_ guard-num] (re-matches #"^Guard #(\d+) begins shift$" line)]
    (assoc acc :current-guard guard-num)
    (case line
      "falls asleep"
      (assoc acc :asleep-time timestamp)
      "wakes up"
      (-> acc
          (update (:current-guard acc)
                  (fnil
                   #(concat % (range (.getMinute (:asleep-time acc)) (.getMinute timestamp)))
                   '()))))))

;; this works \o/
;; part1
(as-> (utils/read-input "2018/day4.txt") l
     (map parse-log-line l)
     (sort-by first l)
     (reduce asleep-minutes {} l)
     (dissoc l :current-guard :asleep-time)
     (sort-by (fn [[g minutes]] (count minutes)) > l)
     (first l)
     (* (utils/parse-int (first l))
        (first (first (sort-by second > (frequencies (second l))))))
     )

;; part 2
(as-> (utils/read-input "2018/day4.txt") l
  (map parse-log-line l)
  (sort-by first l)
  (reduce asleep-minutes {} l)
  (dissoc l :current-guard :asleep-time)
  (map (fn [[g minutes]] (conj (->> (frequencies minutes)
                                    (sort-by second >)
                                    (first)
                                    (reverse)) g)) l)
  (sort-by second > l)
  (first l)
  (* (utils/parse-int (first l)) (last l))
  )

                              )(sort-by second > (frequencies %)) )
  )
