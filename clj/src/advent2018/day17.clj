(ns advent2018.day17
  (:require [utils :as utils]
            [clojure.set :as set]
            [clojure.string :as string]))

(set/union #{1 2 3 4} [4 5 6])

(defn parse-line [s]
  (if-let [line (re-matches #"^x=(\d+), y=(\d+)\.\.(\d+)$" s)]
    (->> line (rest) (map utils/parse-int)
         ((fn ([[x ystart yend]]
               (map #(vector x %) (range ystart (inc yend)))))))
    (if-let [line (re-matches #"^y=(\d+), x=(\d+)\.\.(\d+)$" s)]
      (->> line (rest) (map utils/parse-int)
           ((fn ([[y xstart xend]]
                 (map #(vector % y) (range xstart (inc xend)))))))
      (throw (Exception. (format "Could not parse %s" s))))))

(parse-line "x=495, y=2..7")

(defn parse-clay [filename]
  (->> filename
       (utils/read-input)
       (map parse-line)
       (map set)
       (reduce set/union #{})))

(defn move [[wx wy] dir]
  (case dir
    :down [wx (inc wy)]
    :left [(dec wx) wy]
    :right [(inc wx) wy]))

(peek [1 2 3 4])

(defn absent? [coll key]
  (not (contains? coll key)))

(defn ray [blocked wcoords dir]
  (loop [wcoords wcoords
         result #{}]
    (if (absent? blocked (move wcoords :down))
      [:spills result wcoords]
      (let [wnext (move wcoords dir)]
        (if (contains? blocked (move wcoords dir))
          [:settles (conj result wcoords) nil]
          (recur wnext (conj result wcoords)))))))

(defn add-water [state]
  (let [{:keys [max-y blocked seen at-rest]} state]
    (loop [processed #{}
           queue [[500 0]]
           seen seen
           next-blocked #{}]
      (if (empty? queue)
        (-> state
            (assoc :seen seen)
            (update :blocked (partial set/union next-blocked))
            (update :at-rest (partial set/union next-blocked)))
        (let [wcoords (first queue)
              queue (subvec queue 1)
              wdown (move wcoords :down)]
      ;; if the water can go down, it does.
      ;; otherwise, we flood-fill left and right to see if we settle.
      ;; if we don't settle, we add the flood-fill to seen, and add the
      ;; "drop points" to the queue.
          (if (absent? blocked wdown)
        ;; we head down.
            (if (> (second wdown) max-y)
              (recur processed
                     queue
                     (conj seen wcoords)
                     next-blocked)
              (recur processed
                     (conj queue wdown)
                     (conj seen wcoords)
                     next-blocked))
        ;; otherwise flood-fill left/right from this position, this COULD lead
        ;; to more stuff on the queue.
        ;; both left and right need to settle to end
        ;; otherwise, we just add everything to seen and insert whichever of
        ;; left-coord/right-coord aren't nil to the queue.
            (if (contains? processed wcoords)
              (recur processed queue seen next-blocked)
              (let [[left-result left-seen left-coord] (ray blocked wcoords :left)
                    [right-result right-seen right-coord] (ray blocked wcoords :right)]
                (if (and (= left-result :settles) (= right-result :settles))
                ;; (do
                ;;   (println "water settles" left-seen right-seen)
                  (recur
                   (conj processed wcoords)
                   queue
                   (set/union seen left-seen right-seen)
                   (set/union next-blocked left-seen right-seen))
                  (recur
                   (conj processed wcoords)
                   (cond-> queue
                     left-coord (conj left-coord)
                     right-coord (conj right-coord))
                   (set/union seen left-seen right-seen)
                   next-blocked))))))))))

;; (defn next-square [clay [wx wy]]
;;   ;; if it CAN go down, it will.
;;   ;; if it CAN'T go down, it makes a random left or right movement.
;;   ;; this method doesn't take other water into account.
;;   ;; when "ticking" the full system we will have the water all choose
;;   ;; next spot.
;;   (let [down (move [wx wy] :down)]
;;     (if (not (contains? clay down))
;;       down
;;       (let [move-options (->> '(:left :right)
;;                               (map (partial move [wx wy]))
;;                               (remove (partial contains? clay)))]
;;         (if (empty? move-options)
;;           [wx wy]
;;           (rand-nth move-options))))))

;; (defn tick [clay max-y {:keys [water remaining-water water-countdown]}]
;;   (let [next-water (reduce
;;                     (fn [water wcoords]
;;                       (let [wnext (next-square clay wcoords)
;;                             [wx wy] wnext]
;;                         (cond
;;                           (contains? water wnext) water
;;                           (> wy max-y) (disj water wcoords)
;;                           :else (-> water
;;                                     (disj wcoords)
;;                                     (conj wnext)))))
;;                     water
;;                     water)]
;;     (if (or (not= water-countdown 0)
;;             (= remaining-water 0)
;;             (contains? next-water [500 0]))
;;       {:water next-water
;;        :remaining-water remaining-water
;;        :water-countdown (max 0 (dec water-countdown))}
;;       {:water (conj next-water [500 0])
;;        :remaining-water (dec remaining-water)
;;        :water-countdown 3})))

(defn print-sand [{:keys [min-y max-y clay blocked seen]}]
  (let [max-x (reduce max (map first (set/union seen blocked)))
        min-x (reduce min (map first (set/union seen blocked)))]
    (string/join "\n"
                 (for [y (range min-y (inc max-y))]
      (string/join
       (for [x (range min-x (inc max-x))]
        ;; (do (println [x y])
        (cond
        (contains? clay [x y]) \#
        (contains? blocked [x y]) \~
        (contains? seen [x y]) \|
        :else \.)))))))

(defn answer-part1 [filename]
  (let [clay (parse-clay filename)
        min-y (reduce min (map second clay))
        max-y (reduce max (map second clay))]
    (->> (iterate add-water {:max-y max-y :min-y min-y :blocked clay
                             :clay clay :seen #{} :at-rest #{}})
         (drop 800)
         (first)
      ;;  (print-sand)
      ;;  (println)))
         :seen
         (filter (fn [[x y]] (<= min-y y max-y)))
         (count))))

(answer-part1 "2018/day17-example.txt")
(answer-part1 "2018/day17.txt")


(defn answer-part2 [filename]
  (let [clay (parse-clay filename)
        min-y (reduce min (map second clay))
        max-y (reduce max (map second clay))
        steady-state (->> (iterate add-water {:max-y max-y :min-y min-y :blocked clay
                                              :clay clay :seen #{}
                                              :at-rest #{}})
                          (drop 800)
                          (first))]
    (count (:at-rest steady-state))))

(answer-part2 "2018/day17.txt")
