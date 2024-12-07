(ns aoc2024.util
  (:require [clojure.math]
            [clojure.string])
  (:import (java.io BufferedReader StringReader)))

(defn multi-line [& strings] (clojure.string/join "\n" strings))

(defn unzip [input]
  (for [iter (iterate (partial map rest) input)
        :while (every? seq iter)]
    (map first iter)))

(defn zip [colls]
  (into [] (partition (count colls) (apply interleave colls))))

(defn count-occurrences [s slist]
  (->> slist
       flatten
       (filter #{s})
       count))

(defn string-reader [str]
  (BufferedReader. (StringReader. str)))

(defn inspect
  ([arg]
   (println arg) arg)
  ([message arg]
   (println message arg) arg))

(defn vec-remove
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn sum
  ([coll]     (reduce + coll))
  ([val coll] (reduce + val coll)))

(defn find-all
  [f coll]
  (filter f coll))

(defn find-first
  [f coll]
  (first (filter f coll)))

(defn middle [coll]
  (nth coll (/ (count coll) 2)))

(defn int-sign [v] (int (clojure.math/signum v)))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn num-digits [n]
  (loop [count 0
         remainder n]
    (if (> remainder 0)
      (recur (inc count) (int (/ remainder 10)))
      count)))
