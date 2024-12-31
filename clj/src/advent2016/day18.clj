(ns advent2016.day18
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]
            [utils :as utils]))

(defn next-row [^String prev-row]
  (let [x-max (count prev-row)]
    (string/join
     (for [x (range x-max)]
      (match (vec (for [dx [-1 0 1]]
                    (let [nx (+ x dx)]
                      (if (< -1 nx x-max)
                        (.charAt prev-row nx)
                        \.))))
        [\^ \^ \.] \^
        [\. \^ \^] \^
        [\^ \. \.] \^
        [\. \. \^] \^
        :else \.)))))

(iterate next-row  "..^^.")

(defn count-safe [first-row num-rows]
  (->> first-row
       (iterate next-row)
       (take num-rows)
       (map seq)
       (map #(filter (partial = \.) %))
       (map #(count %))
       (reduce +)))

(count-safe ".^^.^.^^^^" 10)
(count-safe (first (utils/read-input "2016/day18.txt")) 40)

;; so presumably there is some loop.

;; (defn count-safe [first-row num-rows]
;;   (->> first-row
;;        (iterate next-row)
;;        (take num-rows)
;;        (map-indexed vector)
;;        (reduce (fn [acc [n row]]
;;                  (if (contains? acc row)
;;                    (reduced [(acc row) n])
;;                    (assoc acc row n)))
;;                {})))

;; (nth (iterate next-row ".^^.^.^^^^") 62)

;; ;; (println (count-safe ".^^.^.^^^^" 400000))

(println (count-safe (first (utils/read-input "2016/day18.txt")) 400000))
