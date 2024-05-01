(ns advent2016.day22
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]))

(.split #"\s+" "/dev/grid/node-x0-y0     91T   66T    25T   72%")

(def line-re #"^/dev/grid/node\-x(\d+)\-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T.*$")

(defn parse-line [s]
    (if-let [[m & l] (re-matches line-re s)]
      ((fn [[x y size used avail]]
        {[x y] {:size size
                :used used
                :avail avail}})
      (map utils/parse-int l))
      {}))

(parse-line "/dev/grid/node-x0-y0     91T   66T    25T   72%")

(->>
 (combo/combinations (->> (utils/read-input "2016/day22.txt")
                          (map parse-line)
                          (reduce merge)) 2)
 (take 5)
 (filter (fn [[[_ m1] [_ m2]]]
            (or (<= (:used m1) (:avail m2))
                (<= (:avail m1) (:used m2))))))
