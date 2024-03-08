(ns advent2020.day4
  (:require [utils :as utils]
            [clojure.string :as string]))

(defn parse-passport [line]
  (->> line
       (#(string/split % #" "))
       (map (fn [s] (apply hash-map (string/split s #":"))))
       (into {})))



(def part1-required-fields
  #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn is-valid? [passport required-fields]
  (empty? (filter nil? (map passport required-fields))))

(defn answer-part1 [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (partition-by (partial = ""))
       (remove (partial = (list "")))
       (map (partial string/join " "))
       (map parse-passport)
       (map #(is-valid? % part1-required-fields))
       (filter true?)
       (count)))

(answer-part1 "day4-example.txt")
(answer-part1 "day4.txt")

(defn validate-field [field value]
  (try
    (case field
      "byr" (<= 1920 (utils/parse-int value) 2002)
      "iyr" (<= 2010 (utils/parse-int value) 2020)
      "eyr" (<= 2020 (utils/parse-int value) 2030)
      "hgt"
      (let [[m hgt-str unit] (re-matches #"^(\d+)(in|cm)$" value)
            hgt (utils/parse-int hgt-str)]
        (and m
             (case unit
               "in" (<= 59 hgt 76)
               "cm" (<= 150 hgt 193))))
      "hcl" (boolean (re-matches #"#[0-9a-f]{6}" value))
      "ecl" (boolean (re-matches #"(amb|blu|brn|gry|grn|hzl|oth)" value))
      "pid" (boolean (re-matches #"\d{9}" value))
      "cid" true)
    (catch NumberFormatException _ false)))

(defn validate-passport [passport]
  (->> passport
       (map #(apply validate-field %))
       (every? true?)))

(defn answer-part2 [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (partition-by (partial = ""))
       (remove (partial = (list "")))
       (map (partial string/join " "))
       (map parse-passport)
       (filter #(is-valid? % part1-required-fields))
       (map validate-passport)
       (filter true?)
       (count)))

(->> "eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
     (parse-passport)
     (map #(apply validate-field %)))

(answer-part2 "day4-example.txt")
(answer-part2 "day4-example2.txt")
(answer-part2 "day4-example3.txt")
(answer-part2 "day4.txt")
