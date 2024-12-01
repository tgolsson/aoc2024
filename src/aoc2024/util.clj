(ns aoc2024.util
  (:import (java.io BufferedReader StringReader)))

(defn unzip [input]
  (for [iter (iterate (partial map rest) input)
        :while (every? seq iter)]
    (map first iter)))

(defn zip [colls]
  (partition (count colls) (apply interleave colls)))

(defn count-occurrences [s slist]
  (->> slist
       flatten
       (filter #{s})
       count))

(defn string-reader [str]
  (BufferedReader. (StringReader. str)))
