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
  (keys passport))


(->> (utils/read-input "2020/day4-example.txt")
     (partition-by (partial = ""))
     (remove (partial = (list "")))
     (map (partial string/join " "))
     (map parse-passport))
