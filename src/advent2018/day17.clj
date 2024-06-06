(ns advent2018.day17
  (:require [utils :as utils]
            [clojure.set :as set]))

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

;; our approach will be a simulation, with a water molecule "ticking"
;; forward and making a random left/right movement in places where it is
;; blocked.
;; we'll just simulate a bunch of the water molecules and get to the right
;; answer that way.

(defn move [[wx wy] dir]
  (case dir
    :down [wx (inc wy)]
    :left [(dec wx) wy]
    :right [(inc wx) wy]))

(rand-nth '())

(defn next-square [clay [wx wy]]
  ;; if it CAN go down, it will.
  ;; if it CAN'T go down, it makes a random left or right movement.
  ;; this method doesn't take other water into account.
  ;; when "ticking" the full system we will have the water all choose
  ;; next spot.
  (let [down (move [wx wy] :down)]
    (if (not (contains? clay down))
      down
      (let [move-options (->> '(:left :right)
                              (map (partial move [wx wy]))
                              (remove (partial contains? clay)))]
        (if (empty? move-options)
          [wx wy]
          (rand-nth move-options))))))

(defn tick [clay max-y {:keys [water remaining-water water-countdown]}]
  (let [next-water (reduce
                    (fn [water wcoords]
                      (let [wnext (next-square clay wcoords)
                            [wx wy] wnext]
                        (cond
                          (contains? water wnext) water
                          (> wy max-y) (disj water wcoords)
                          :else (-> water
                                    (disj wcoords)
                                    (conj wnext)))))
                    water
                    water)]
    (if (or (not= water-countdown 0)
            (= remaining-water 0)
            (contains? next-water [500 0]))
      {:water next-water
       :remaining-water remaining-water
       :water-countdown (max 0 (dec water-countdown))}
      {:water (conj next-water [500 0])
       :remaining-water (dec remaining-water)
       :water-countdown 3})))


(let [clay (parse-clay "2018/day17-example.txt")
      max-y (reduce max (map second clay))]
  (->> (iterate (partial tick clay max-y)
                {:water #{} :remaining-water 5
                 :water-countdown 0})
       (take 100)
       (reduce (fn [seen {:keys [water]}]
                 (set/union seen water))
               #{})
       (count)))
