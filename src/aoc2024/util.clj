(ns aoc2024.util
  (:import (java.io BufferedReader StringReader)))

(defn multi-line [& strings] (clojure.string/join "\n" strings))

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

(defn inspect [arg]
  (println arg)
  arg)

(defn vec-remove
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))
