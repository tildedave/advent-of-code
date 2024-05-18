(ns advent2018.day4
  (:require [utils :as utils]))

(def formatter (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))

;; this works \o/
(as-> (utils/read-input "2018/day4.txt") l
     (map #(let [[_ timestamp r] (re-matches #"\[([^]]+)\] (.*)$" %)]
             [(java.time.LocalDateTime/parse timestamp formatter) r])
          l)
     (sort-by first l)
     (reduce
      (fn [acc [timestamp line]]
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
      {}
      l
      )
     (dissoc l :current-guard :asleep-time)
     (sort-by (fn [[g minutes]] (count minutes)) > l)
     (first l)
     (* (utils/parse-int (first l))
        (first (first (sort-by second > (frequencies (second l))))))
    ;;  (* (utils/parse-int (first l)) (first (sort-by second > (frequencies (second l)))))
     )
