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
    "49 53 8"
    "0 11 42"
    "42 0 7"
    "57 7 4"
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
             (let [[[dest-start dest-end] [source-start source-end]] matched-interval]
               (+ (- num source-start) dest-start))))
           (get-in parsed-almanac [:maps type :dest]))))))

(seed-destination (parse-almanac example-almanac) 79)

(defn answer-part1 [parsed-almanac]
  (->> (:seeds parsed-almanac)
       (map (partial seed-destination parsed-almanac))
       (reduce min)))

(answer-part1 (parse-almanac example-almanac))
(answer-part1 (parse-almanac (utils/read-input "2023/day5.txt")))

;; OK for part 2 we need to go backwards and keep track of the intervals
;; or something.

