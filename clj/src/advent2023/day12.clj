(ns advent2023.day12
  (:require [utils :as utils]
            [clojure.string :as string]))

;; perhaps this is the problem where I will finally use core.logic.
;; well, I guess not.  core.logic would be great for finding solutions,
;; but we eventually need to count solutions.  I'm sure core.logic can
;; be used for that too but I'm too rusty ;-)

(defn parse-conditions [^String line]
  (let [[spring-conditions num-list] (.split line " ")]
    [spring-conditions (utils/parse-number-list num-list #",")]))

(parse-conditions "???.### 1,1,3")

;; this will be some kind of recursive/memoized algorithm, I guess.
;; a very naive approach would compute them all, but if we look at my
;; answers from the golang solution, the numbers get too big :-)
;; my golang approach ended up being pretty awful.  for Clojure I hope
;; I can make it better :-)
;; we can approach from either direction (left/right), which seems like an
;; optimization vs something fundamental towards a solution.


(defn can-force-contiguous? [^String s n]
  (if (> n (count s))
    false
    (let [group (subs s 0 n)]
    (and
     (every? (partial contains? #{\# \?}) group)
     (or
      (= (count s) n) ;; don't need to look at n + 1th
      (not= (.charAt s n) \#))))))

;; memoized via monad, sort of bleh
;; I don't like either of the memoization strategies that I'm familiar with.
(def num-arrangements
  (let [_num-arrangements
        (fn [mem-num-arrangements record group-sizes]
          (let [rec (partial mem-num-arrangements mem-num-arrangements)]
            (if (empty? record)
              (if (empty? group-sizes)
                1
                0)
              (case (.charAt record 0)
                \# (if-let [s (first group-sizes)]
                     (if (can-force-contiguous? record s)
                       (rec
                        (subs record (min (inc s) (count record)))
                        (rest group-sizes))
                       0)
                     0)
                \. (rec (subs record 1) group-sizes)
                \? ;; either we choose 0 or 1
                (+
                 (rec (str "." (subs record 1)) group-sizes)
                 (rec (str "#" (subs record 1)) group-sizes))))))
        mem-num-arranagements (memoize _num-arrangements)]
          (partial mem-num-arranagements mem-num-arranagements)))

(defn total-arrangements [lines]
  (->> (map parse-conditions lines)
       (map (partial apply num-arrangements))
       (reduce +)))

(total-arrangements '("???.### 1,1,3"
                      ".??..??...?##. 1,1,3"
                      "?#?#?#?#?#?#?#? 1,3,1,6"
                      "????.#...#... 4,1,1"
                      "????.######..#####. 1,6,5"
                      "?###???????? 3,2,1"))

;; correct \o/
(total-arrangements (utils/read-input "2023/day12.txt"))

(defn unfold [[spring-conditions num-list]]
  [(string/join "?" (repeat 5 spring-conditions))
  (flatten (repeat 5 num-list))])

(defn arrangements-part2 [s]
  (->> s (parse-conditions) (unfold) (apply num-arrangements)))

(defn total-arrangements-part2 [lines]
  (->> lines
       (map arrangements-part2)
       (reduce +)))

;; correct
(total-arrangements-part2
'("???.### 1,1,3"
  ".??..??...?##. 1,1,3"
  "?#?#?#?#?#?#?#? 1,3,1,6"
  "????.#...#... 4,1,1"
  "????.######..#####. 1,6,5"
  "?###???????? 3,2,1"))

;; correct \o/
(time (total-arrangements-part2 (utils/read-input "2023/day12.txt")))
