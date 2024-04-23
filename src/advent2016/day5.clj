(ns advent2016.day5
  (:require [clojure.string :as string]
            [utils :as utils]))

(def md (java.security.MessageDigest/getInstance "MD5"))
;; (.digest md (.getBytes (format "%x" 3231929)))

;; (format "%x" 15)
;; (format "%x" 3231929)


(string/join [\a \b \c])

(every? #(not (nil? %)) [\a \b \c \d])

(defn hex-digest [md s]
  (format "%1$032x" (BigInteger. 1 (.digest md (.getBytes s)))))

(= (take 5 (hex-digest md "abc3231929")) '(\0 \0 \0 \0 \0))
(nth (hex-digest md "abc3231929") 5)

(vec (repeat 8 nil))

(defn answer-part1 [prefix]
  (let [md (java.security.MessageDigest/getInstance "MD5")]
    (loop [n 0 passwd []]
      (let [hexed (hex-digest md (format "%s%d" prefix n))]
        (if (= (take 5 hexed) '(\0 \0 \0 \0 \0))
          (let [passwd (conj passwd (nth hexed 5))]
            (if (= (count passwd) 8)
              (string/join passwd)
              (recur (inc n) passwd)))
          (recur (inc n) passwd))))))

;; (println (answer-part1 "abc"))
;; (println (answer-part1 (first (utils/read-input "2016/day5.txt") )))

(every? #(not (nil? %)) [\0 \5 \a \c \e \8 \e \3])

(defn answer-part2 [prefix]
  (let [md (java.security.MessageDigest/getInstance "MD5")]
     (loop [n 0 passwd (vec (repeat 8 nil))]
       (let [hexed (hex-digest md (format "%s%d" prefix n))]
         (if (= (take 5 hexed) '(\0 \0 \0 \0 \0))
           (let [pos (nth hexed 5)
                 ch (nth hexed 6)
                 int-pos (utils/try-parse-int (str pos))]
             (if (and (contains? #{\0 \1 \2 \3 \4 \5 \6 \7} pos)
                      (nil? (get passwd int-pos)))
               (let [passwd (assoc passwd int-pos ch)]
                 (if (every? #(not (nil? %)) passwd)
                   (string/join passwd)
                   (recur (inc n) passwd)))
               (recur (inc n) passwd)))
           (recur (inc n) passwd))))))

;; (println (answer-part2 "abc"))
(println (answer-part2 (first (utils/read-input "2016/day5.txt") )))
