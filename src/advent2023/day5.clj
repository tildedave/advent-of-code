(ns advent2023.day5
  (:require [utils :as utils]
            [intervals :as intervals]))

;; so this is one of those interval puzzles.
;; we can see if the interval code I wrote makes this less of a slog.

(defn interval-contains? [[lo hi] n]
  (<= lo n (dec hi)))

(defn parse-almanac-map [lines]
  (let [[header & lines] lines
        [src dest] (rest (re-matches #"^(\w+)-to-(\w+) map:$" header))]
    {src
     {:dest dest
      :ranges (map #(let [[dest source length] (utils/parse-number-list %)]
                       [[dest (+ dest length)]
                        [source (+ source length)]])
                    lines)}}))

(parse-almanac-map
 '("seed-to-soil map:"
   "50 98 2"
   "52 50 48"))

(parse-almanac-map
 '("soil-to-fertilizer map:"
   "0 15 37"
   "37 52 2"
   "39 0 15"))

(def example-almanac
  '("seeds: 79 14 55 13"
    ""
    "seed-to-soil map:"
    "50 98 2"
    "52 50 48"
    ""
    "soil-to-fertilizer map:"
    "0 15 37"
    "37 52 2"
    "39 0 15"
    ""
    "fertilizer-to-water map:"
    "49 53 8"  ;; [53 61] -> [49 57]
    "0 11 42"  ;; [11 53] -> [0 42]
    "42 0 7"   ;; [0 7]   -> [42 49]
    "57 7 4"   ;; [7 11]  -> [57 61]
    ""
    "water-to-light map:"
    "88 18 7"
    "18 25 70"
    ""
    "light-to-temperature map:"
    "45 77 23"
    "81 45 19"
    "68 64 13"
    ""
    "temperature-to-humidity map:"
    "0 69 1"
    "1 0 69"
    ""
    "humidity-to-location map:"
    "60 56 37"
    "56 93 4"))

(second (.split "seeds: 79 14 55 13" ":"))

(defn parse-seeds [line]
  (utils/parse-number-list (second (.split line ": "))))

(defn parse-almanac [lines]
  (let [[[seeds] & maps] (utils/split-by "" lines)]
    {:seeds (parse-seeds seeds) ;; you need to parse these actually
     :maps (reduce merge (map parse-almanac-map maps))}))

(parse-almanac example-almanac)

(defn seed-destination [parsed-almanac seed]
  (loop [num seed
         type "seed"]
    (if (= type "location")
      num
      (let [current-ranges (get-in parsed-almanac [:maps type :ranges])]

        (recur
         (let [matched-interval (->> (filter #(interval-contains? (second %) num) current-ranges)
                                     (first))]
           (if (nil? matched-interval)
             num
             (let [[[dest-start _] [source-start _]] matched-interval]
               (+ (- num source-start) dest-start))))
           (get-in parsed-almanac [:maps type :dest]))))))

(seed-destination (parse-almanac example-almanac) 79)

(defn answer-part1 [parsed-almanac]
  (->> (:seeds parsed-almanac)
       (map (partial seed-destination parsed-almanac))
       (reduce min)))

(answer-part1 (parse-almanac example-almanac))
(answer-part1 (parse-almanac (utils/read-input "2023/day5.txt")))

;; OK for part 2 we need to keep track of the intervals or something.
;; one way to do this would be to just expand seed-destination so that
;; it operates on an interval instead of a single number.

(intervals/contained? [3 4] [2 5])

(defn apply-map-to-interval [[start end] [[dstart dend] [sstart send]]]
;; 4 possibilities.
;; clips left (split off part to the left as unmapped)
;; clips right (split off part to the right as unmapped)
;; map totally contains the interval, in which case we map it all
;; interval totally contains the map, in which case we map the section
;; and clip the sides.
  (let [{:keys [mapped unmapped]}
    ;; OK we can assume the intervals match now
        (cond
      ;; case 1, clips left  (assumed end >= sstart)
      ;; might get into some off by 1s with the end.
          (and (< start sstart) (<= end send))
          {:mapped [sstart (min end send)]
           :unmapped [[start sstart]]}
      ;; case 2, clips right
          (and (<= sstart start) (< start send) (<= send end))
          {:mapped [start send]
           :unmapped [[send end]]}
      ;; case 3, range we're mapping is completely contained
          (<= sstart start end send)
          {:mapped [start end]
           :unmapped []}
      ;; case 4, range we're mapping completely contains the map
          (< start sstart send end)
          {:mapped [sstart send]
           :unmapped [[start sstart] [send end]]}
          :else (throw (Exception. "did not fall into one of my cases")))
        [mstart mend] mapped
        offset (- mstart sstart)
        length (- mend mstart)]
    [[(+ dstart offset)
      (+ dstart offset length)] unmapped]))

(defn interval-length [[x y]] (- y x))

(defn map-interval [interval ranges]
  ;; so the interval is turned into a list of mapped intervals and a list of
  ;; remaining intervals
  (->>
   (loop
    [intervals [interval]
     mapped-intervals []
     unmapped-intervals []]
     (if-let [interval (first intervals)]
       (if-let [overlapping-interval (->> ranges
                                          (filter #(intervals/overlap? (second %) interval))
                                          (first))]
         (let [[mapped-portion unmapped-portions] (apply-map-to-interval interval overlapping-interval)]
           (recur
            (reduce conj (rest intervals) unmapped-portions)
            (conj mapped-intervals mapped-portion)
            unmapped-intervals))
         (recur
          (rest intervals)
          mapped-intervals
          (conj unmapped-intervals interval)))
       (concat mapped-intervals unmapped-intervals)))
   (remove (fn [[x y]] (= x y)))
   (sort-by first)
   (reduce
    (fn [[all prev] current]
      (cond
        (nil? prev) [all current]
        (= (second prev) (first current))
        [all [(first prev) (second current)]]
        :else [(conj all prev) current]))
    [[] nil])
   ((fn [[all current]] (conj all current)))))

(map-interval [0 70] [[[50 52] [52 54]]])

(defn seed-ranges [almanac]
  (map (fn [[start length]] [start (+ start length)]) (partition 2 (:seeds almanac))))

(seed-ranges (parse-almanac example-almanac))

(defn seed-range-destination [parsed-almanac seed-range]
  (loop [ranges [seed-range]
         type "seed"]
    (if (= type "location")
      ranges
      (let [current-ranges (get-in parsed-almanac [:maps type :ranges])]
        (recur
         (mapcat
          #(map-interval % current-ranges)
          ranges)
         (get-in parsed-almanac [:maps type :dest]))))))

;; this is right
;; I guess that doesn't mean too much as it will only handle the
;; "seed is contained" case.
(let [parsed-almanac (parse-almanac (utils/read-input "2023/day5.txt"))]
  (->>
   (for [seed (:seeds parsed-almanac)]
    (seed-range-destination parsed-almanac [seed (inc seed)]))
   (map first)
   (map first)
   (reduce min)))

(defn answer-part2 [parsed-almanac]
  (->> parsed-almanac
       (seed-ranges)
       (map (partial seed-range-destination parsed-almanac))
       (map #(->> %
                  (map first)
                  (reduce min)))
       (reduce min)
  ))

(answer-part2 (parse-almanac example-almanac))

(let [parsed-almanac (parse-almanac example-almanac)]
  (->> (range 55 (+ 55 13))
       (map (partial seed-destination parsed-almanac))
       (sort)))
;; wrong wrong wrong
(answer-part2 (parse-almanac (utils/read-input "2023/day5.txt")))
