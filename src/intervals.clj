(ns intervals)

(defn overlap? [[lo1 hi1] [lo2 hi2]]
  (and (< lo1 hi2)
       (< lo2 hi1)))

(defn interval-< [[lo1 hi1] [lo2 hi2]]
  (< hi1 lo2))

(defn interval-> [[lo1 hi1] [lo2 hi2]]
  (< hi2 lo1))

(defn interval-merge [[lo1 hi1] [lo2 hi2]]
  (if (overlap? [lo1 hi1] [lo2 hi2])
    [(min lo1 lo2) (max hi1 hi2)]
    (throw (Exception. "asked to merge two non-overlapping intervals"))))

;; OK RBT time seems to have worked.

(defn interval-compare [i1 i2]
  (cond
    (interval-< i1 i2) -1
    (interval-> i1 i2) 1
    :else 0))

;; this is the non-rbt approach, I'm going to use it

(defn interval-list-merge [intervals]
  (loop [intervals intervals
         result '[]]
    (if-let [x (first intervals)]
      (recur
       (rest intervals)
       (if (empty? result)
         [x]
         (let [q (group-by (partial interval-compare x) result)]
           (-> (get q 1 [])
               (into [(reduce interval-merge x (q 0))])
               (into (q -1))))))
      result)))
