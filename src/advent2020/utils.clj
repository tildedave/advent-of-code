(ns advent2020.utils
  (:require [clojure.java.io :as io]))

(defn read-resource-lines [resource]
  (line-seq (io/reader (io/resource resource))))

(defn read-input [str]
  (read-resource-lines (format "2020/%s" str)))
