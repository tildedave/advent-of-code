(ns advent2022.day17
  (:require [utils :as utils]
            [clojure.set :as set]
            [clojure.string :as string]))

(def line (first (utils/read-resource-lines "2022/day17.txt")))
(defn parse-pattern [jet-pattern-str]
  (mapv #(case % \> :right \< :left) jet-pattern-str))

(def parsed-line (parse-pattern line))

;; y = 0 is the bottom
;; shapes appear 3 y units above highest (so keep track of the highest)
;; question: difference between the at-rest rocks and the shape.
;; I guess we can just do the basic stuff and see how it goes.
;; keep track of the rows of the active shape.

(def shape-order (list :horizontal :plus :bend :vertical :square))

(defn new-cavern [] [(vec (repeat 9 \#))])
(def rock-cavern (new-cavern))
(defn new-row [] (vector \# \. \. \. \. \. \. \. \#))
(new-row)


(peek [1 2 3])

(defn remove-empty-rows [rock-cavern]
  (let [empty-row (new-row)]
    (loop [cavern rock-cavern]
      (if (= empty-row (peek cavern))
        (recur (pop cavern))
        cavern))))

(defn add-shape [shape rock-cavern]
  (let [num-rows (case shape :horizontal 1 :plus 3 :bend 3 :vertical 4 :square 2)
        rock-cavern (remove-empty-rows rock-cavern)
        ;; actually, we should just prune any row at the end that's empty.
        ;; that makes our logic much easier (and the calculations below correct.)
        expanded-cavern (loop [i 0 cavern rock-cavern]
                          (if (>= i (+ num-rows 3))
                            cavern
                            (do
                              (recur (inc i) (conj cavern (new-row))))))
        start-y (+ (dec (count rock-cavern)) num-rows 3)
        coords (case shape
                 :horizontal
                 [[start-y 3] [start-y 4] [start-y 5] [start-y 6]]
                 :plus
                 [[start-y 4] [(- start-y 1) 3] [(- start-y 1) 4] [(- start-y 1) 5] [(- start-y 2) 4]]
                 :bend
                 [[start-y 5] [(- start-y 1) 5] [(- start-y 2) 5] [(- start-y 2) 4] [(- start-y 2) 3]]
                 :vertical
                 [[start-y 3] [(- start-y 1) 3] [(- start-y 2) 3] [(- start-y 3) 3]]
                 :square
                 [[start-y 3] [start-y 4] [(- start-y 1) 3] [(- start-y 1) 4]])]
    [(reduce
      (fn [m r] (assoc-in m r \@))
      expanded-cavern
      coords)
     (dec (reduce min (map first coords))) ;; subtract 1 because we want to look 1 below for fall purposes.
     (reduce max (map first coords))]))


(defn visualize-cavern [rock-cavern]
  (string/join "\n" (map string/join (reverse rock-cavern))))

(defn every-index-of [v item]
  ;; bad perf but failed at trying to fight lazy-seq and the indices we're dealing with are small
  (->> v
       (map-indexed vector)
       (filter #(= (second %) item))
       (map first)))

;; (every-index-of ((add-shape :square rock-cavern) 3) \@)

(range 0 5)
(defn every-index-in-range [rock-cavern ystart yend ch]
  (mapcat
   #(map (fn [i] (vector i %)) (every-index-of (rock-cavern %) ch))
   (range ystart (inc yend))))

;; (every-index-of ((add-shape :vertical rock-cavern) 3) \@)
;; (every-index-in-range (add-shape :vertical rock-cavern) 0 3 \@)

(defn update-locations [rock-cavern coord-list ch]
  (reduce
   (fn [m r] (assoc-in m (reverse r) ch))
   rock-cavern
   coord-list))

;; (defn fall [rock-cavern ystart yend]
;;   ;; ystart/yend are where we look for any @s.
;;   ;; there are going to be a lot of rows and we only want to look at a few.
;;   (let [shape-locations (every-index-in-range rock-cavern ystart yend \@)
;;         rock-locations (every-index-in-range rock-cavern ystart yend \#)
;;         fall-locations (map (fn [[x y]] [x (dec y)]) shape-locations)
;;         fall-blocked (or (some #(= (second %) 0) fall-locations)
;;                          (not-empty (set/intersection (set rock-locations) (set fall-locations))))]
;;     [fall-blocked
;;      (if fall-blocked
;;        (update-locations rock-cavern shape-locations \#)
;;        (-> rock-cavern
;;            (update-locations shape-locations \.)
;;            (update-locations fall-locations \@)))]))

(defn move-shape [rock-cavern ystart yend direction rock?]
  (let [shape-locations (every-index-in-range rock-cavern (max ystart 0) yend \@)
        rock-locations (every-index-in-range rock-cavern (max ystart 0) yend \#)
        next-locations (map
                        (fn [[x y]]
                          [(case direction :right (inc x) :left (dec x) x)
                           (case direction :down (dec y) y)])
                        shape-locations)
        next-blocked (not-empty (set/intersection (set rock-locations) (set next-locations)))]
    [next-blocked
     (if next-blocked
       (if rock? (update-locations rock-cavern shape-locations \#) rock-cavern)
       (-> rock-cavern
           (update-locations shape-locations \.)
           (update-locations next-locations \@)))]))

(defn fall [rock-cavern ystart yend]
  (move-shape rock-cavern ystart yend :down true))

(defn gust [direction]
  (fn [rock-cavern ystart yend]
    (let [[_ rock-cavern] (move-shape rock-cavern ystart yend direction false)]
      rock-cavern)))

;; stepping through the next shape needs to take a list of gusts
;; and return the next list of gusts.

(defn step [[rock-cavern gust-idx shape-seq new-shape? gust? ymin ymax]]
  ;; returns new rock-cavern, new gusts.
  ;; does it know when it's time for a new shape?  it has to.
  ;; this is also what fall returns - if it becomes rock, new shape time.
  (cond
    new-shape?
    (let [[cavern ymin ymax] (add-shape (first shape-seq) rock-cavern)]
      [cavern
       gust-idx
       (rest shape-seq)
       false
       true
       ymin
       ymax])
    gust?
    [((gust (nth parsed-line gust-idx)) rock-cavern ymin ymax)
     (mod (inc gust-idx) (count parsed-line))
     shape-seq
     false
     false
    ;; gusting does not affect ymin/ymax
     ymin
     ymax]
    :else
    (let [[done next-cavern] (fall rock-cavern ymin ymax)]
      [next-cavern
       gust-idx
       shape-seq
       done
       true
       (dec ymin)
       (dec ymax)])))


(first (step (step [(new-cavern) 0 (cycle shape-order) true false -1 -1])))

;; (do
;;   (doall (map-indexed (fn [n x] (println "------- ")
;;                         (println n)
;;                         (println (visualize-cavern (first x)))
;;                         (println "------- ")) (take 2022 (iterate step [(new-cavern) (cycle parsed-line) (cycle shape-order) true false -1 -1]))))
;;   1)

;; answer to part 1
(let [state-seq (iterate step [(new-cavern) 0 (cycle shape-order) true false -1 -1])
      [state-num] (->> state-seq
                       (map-indexed (fn [n [_ _ _ new-shape?]] (if new-shape? [n true] [n false])))
                       (filter #(true? (second %)))
                       (rest) ;; drop index 0 since it is dumb.
                       (take 2022)
                       (last))
      [cavern] (nth state-seq state-num)]
  (println "part 1 answer:" (dec (count (remove-empty-rows cavern)))))

;; uh, I hope part 2 can be solved via some approximation.
;; there must be a cycle at some point.  the shapes cycle and the gusts cycle.
;; we have to start repeating the cavern.
;; the example has a shorter gust pattern but the real version has 10k gusts.
;; cycles will be longer.
;; OK, so, let's convert cavern rows into numbers.
;; then we start looking for a cycle in the numbers.

(defn row-to-number [v]
  (reduce bit-set 0 (->> (map-indexed vector v)
                         (filter #(= (second %) \#))
                         (map first))))

(def search-cycle-cutoff (* (count parsed-line) (count shape-order)))

(defn calculate-cycle-length []
  (let [orig-state-seq (->> (iterate step [(new-cavern) 0 (cycle shape-order) true false -1 -1])
                            (map-indexed (fn [n [_ _ _ new-shape? :as state]] (if new-shape? [n true state] [n false state])))
                            (filter #(true? (second %)))
                            (map (fn [[_ _ state]] state)))] ;; drop index 0 since it is dumb.
    (loop [state-seq orig-state-seq state-set {} n 0]
      (let [[cavern gust-idx [next-shape]] (first state-seq)
            state-hash [(map row-to-number (subvec cavern (max 0 (- (count cavern) 20)))) gust-idx next-shape]]
        (cond
          (> n search-cycle-cutoff) nil
          (contains? state-set state-hash)
          (let [indexed-state-seq (vec (take n orig-state-seq))
                cycle-start (state-set state-hash)
                original-rows (dec (count (remove-empty-rows (first (nth indexed-state-seq cycle-start)))))
                map-with-cycle-order (->> (vals state-set)
                                          (filter #(>= % cycle-start))
                                          (map #(nth indexed-state-seq %))
                                          (map first)
                                          (map #(count (remove-empty-rows %)))
                                          (map #(- % original-rows))
                                          (map dec)
                                          (sort)
                                          (vec))
                rocks-per-cycle (- (dec (count (remove-empty-rows cavern))) original-rows)]
            [(state-set state-hash) n rocks-per-cycle map-with-cycle-order state-set])
          :else
          (recur
           (rest state-seq)
           (assoc state-set state-hash n)
           (inc n)))))))

(defn cavern-after-n-rocks [num-rocks]
  (-> (filter
       #(nth % 3)
       (iterate step [(new-cavern) 0 (cycle shape-order) true false -1 -1]))
      (nth num-rocks)
      (first)))

(defn cavern-height-brute-force [num-rocks]
  (-> (cavern-after-n-rocks num-rocks)
      (remove-empty-rows)
      (count)
      (dec)))

(defn cavern-height [num-rocks]
  (let [[cycle-start cycle-end rocks-per-cycle rock-seq] (calculate-cycle-length)]
    (if (< num-rocks cycle-start)
      (cavern-height-brute-force num-rocks)
      ;; otherwise the answer is based on where we are in the cycle
      (let [cycle-length (- cycle-end cycle-start)
            cycle-idx (mod (- num-rocks cycle-start) cycle-length)
            rocks-at-start (cavern-height-brute-force cycle-start)
            num-full-cycles (quot (- num-rocks cycle-start) cycle-length)
            _ (println "rocks-per-cycle" rocks-per-cycle)
            _ (println "cycle-start" cycle-start)
            _ (println "cycle end" cycle-end)
            ]
        (+
         (* num-full-cycles rocks-per-cycle)
         rocks-at-start
         (nth rock-seq cycle-idx))))))

;; answer to part 2
(println "part 2 answer:" (cavern-height 1000000000000))
