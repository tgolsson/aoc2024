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
      (recur (inc count) (long (/ remainder 10)))
      count)))

(defmacro vx [a]
  `(get ~a 0))

(defmacro vy [a]
  `(get ~a 1))

(defmacro v2+ [a b]
  `[(+ (vx ~a) (vx ~b))
    (+ (vy ~a) (vy ~b))])

(defmacro v2- [a b]
  `[(- (vx ~a) (vx ~b))
    (- (vy ~a) (vy ~b))])

(defmacro get-at [grid pos]
  `(nth
    (nth ~grid (nth ~pos 1)) (nth ~pos 0)))

(defn grid-dimensions [grid]
  [(count (get grid 0)) (count grid)])

(defn inside-grid?
  ([dims position]
   (not (or (< (vx position) 0)
            (< (vy position) 0)
            (>= (vx position) (vx dims))
            (>= (vy position) (vy dims)))))
  ([grid]
   (let [dims (grid-dimensions grid)]
     (fn [position] (inside-grid? dims position)))))

(defn grid-where [grid value]
  (for [y (range (count grid))
        x (range (count (nth grid 0)))
        :let [pos [x y]]
        :when (= (get-at grid pos) value)]
    pos))
