(ns advent2024.day9
  (:require
    [utils :as utils]))

;; we'll use a naive approach and see if that gets us into trouble

(defn expand-disk-format [number-as-string]
  (loop
   [num-seq (->> number-as-string
       (seq)
       (map Character/getNumericValue))
    even? true
    idx 0
    result []]
    (if (empty? num-seq)
      (reduce into [] result)
      (recur
       (rest num-seq)
       (not even?)
       (if even? (inc idx) idx)
       (conj result (repeat (first num-seq)
                            (if even? idx \.))))))

;; then to compcat we'll use two pointers, one from the end, and one from the
;; beginning

(expand-disk-format "12345")

(defn compact-expanded-disk [disk-list]
  (let [walk-right (fn [disk-list n] (->> (range n (count disk-list)) (filter #(= (nth disk-list %) \.)) (first)))
        walk-left (fn [disk-list n] (->> (range n 0 -1) (filter #(not= (nth disk-list %) \.)) (first)))]
    (loop [left (walk-right disk-list 0)
           right (walk-left disk-list (dec (count disk-list)))
           result disk-list]
      (if (> left right)
        result
        ;; otherwise, swap left/right and continue
        (let [next-result (-> result
                              (assoc left (get result right))
                              (assoc right (get result left)))]
          (recur
           (walk-right next-result left)
           (walk-left next-result right)
           next-result))))))

(compact-expanded-disk (expand-disk-format 12345))
(compact-expanded-disk (expand-disk-format 2333133121414131402))

(defn checksum [disk-list]
  (->>
   (map-indexed vector disk-list)
   (map (fn [[n x]] (* n (if (= x \.) 0 x))))
   (reduce +)))

(checksum (compact-expanded-disk (expand-disk-format "2333133121414131402")))

(expand-disk-format (first (utils/read-input "2024/day9.txt")))

(checksum (compact-expanded-disk (expand-disk-format (first (utils/read-input "2024/day9.txt")))))
