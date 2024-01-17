(ns advent2022.utils
  (:require [clojure.java.io :as io]))

(defn read-resource-lines [resource]
  (line-seq (io/reader (io/resource resource))))

(defn parse-int [str] (Integer/valueOf str))
