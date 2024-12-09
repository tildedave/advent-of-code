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
                            (if even? idx \.)))))))

;; then to compcat we'll use two pointers, one from the end, and one from the
;; beginning

(expand-disk-format "12345")

(defn compact-part1 [disk-list]
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

(compact-part1 (expand-disk-format "12345"))
(compact-part1 (expand-disk-format "2333133121414131402"))

(defn checksum [disk-list]
  (->>
   (map-indexed vector disk-list)
   (map (fn [[n x]] (* n (if (= x \.) 0 x))))
   (reduce +)))

(checksum (compact-part1 (expand-disk-format "2333133121414131402")))

(expand-disk-format (first (utils/read-input "2024/day9.txt")))

(checksum (compact-part1 (expand-disk-format (first (utils/read-input "2024/day9.txt")))))

;; a different rep would make this easier, something not naive
;; let's just stick with the same rep, as dumb as this is, the updates will be
;; a bit easier
;; this is essentially selection sort.  list size is 20k.  should be fine
;; I suppose can memoize the blocks we find and invalidate them.

(defn find-start-of-block [disk-list n]
  (let [start (nth disk-list n)
        result (->> (range n 0 -1)
                    (drop-while #(= (nth disk-list %) start))
                    (first))]
    (if (nil? result) nil (inc result))))

(defn find-free-block [disk-list lower upper n]
  ;; here's where we can memoize maybe
  ;; can add upper bound easily
  (loop [start lower]
    (let [next-free-block
          (->> (range start upper)
               (drop-while #(not= (nth disk-list %) \.))
               (take-while #(= (nth disk-list %) \.)))]
      (cond
        (empty? next-free-block) nil
        (>= (count next-free-block) n) (first next-free-block)
        :else (recur (inc (last next-free-block)))))))

(defn compact-part2 [disk-list]
  (let [walk-right (fn [disk-list n] (->> (range n (count disk-list)) (filter #(= (nth disk-list %) \.)) (first)))
        walk-left (fn [disk-list n] (->> (range n 0 -1) (filter #(not= (nth disk-list %) \.)) (first)))]
    (loop
     [disk-list disk-list
      left (walk-right disk-list 0)
      right (walk-left disk-list (dec (count disk-list)))]
      (if-let [block-start (find-start-of-block disk-list right)]
        (let [_ (assert (not= (get disk-list block-start) \.))
              size-needed (- (inc right) block-start)
              start-free-block (find-free-block disk-list left right size-needed)]
          (if (nil? start-free-block)
            (recur
             disk-list
             left
             (walk-left disk-list (dec block-start)))
            (let [free-positions (range start-free-block (+ start-free-block size-needed))
                  move-positions (range block-start (inc right))
                  _ (assert (= (count free-positions) (count move-positions)))
                  next-disk-list (reduce
                                  (fn [disk-list [to from]]
                                    (-> disk-list
                                        (assoc to (get disk-list from))
                                        (assoc from (get disk-list to))))
                                  disk-list
                                  (map vector free-positions move-positions))]
              (recur
               next-disk-list
               (walk-right next-disk-list left)
               (walk-left next-disk-list block-start)))))
        disk-list))))

(time (checksum (compact-part2 (expand-disk-format "2333133121414131402"))))
(time (checksum (compact-part2 (expand-disk-format (first (utils/read-input "2024/day9.txt"))))))
