(ns aoc2024.day7
  (:require [aoc2024.util :refer :all]
            [clojure.string]
            [clojure.math]
            [clojure.java.io]
            [clojure.set]))

(def example
  (multi-line
   "190: 10 19"
   "3267: 81 40 27"
   "83: 17 5"
   "156: 15 6"
   "7290: 6 8 6 15"
   "161011: 16 10 13"
   "192: 17 8 14"
   "21037: 9 7 18 13"
   "292: 11 6 16 20"))

(defn split-parse [sep content]
  (mapv Long/parseLong (clojure.string/split content (re-pattern sep))))

(defn parse [rdr]
  (->> (line-seq rdr)
       (map (fn [line] (vec (clojure.string/split line #": "))))
       (mapv (fn [[tv values]]  [(Long/parseLong tv) (split-parse " "  values)]))))

(defn int-cat [pre post]
  (+ (* (exp 10 (num-digits post)) pre) post))

(defn pick-one-recurse [tv value rem fs]
  (cond
    (empty? rem) (= value tv)
    (> value tv) false
    :else (let [head (nth rem 0)
                tail (rest rem)]
            (not (not-any? #(pick-one-recurse tv (% value head) tail fs) fs)))))

(defn solve [tests]
  (let [valid (group-by (fn [[tv values]] (pick-one-recurse tv (nth values 0) (rest values) [+ *])) tests)
        part1 (->> (get valid true)
                   (map #(nth % 0))
                   sum)]
    [part1
     (+ part1 (->> (get valid false)
                   (filter (fn [[tv values]] (pick-one-recurse tv (nth values 0) (rest values) [+ * int-cat])))
                   (map #(nth % 0))
                   sum))]))

(defn -main []
  (assert (= (inspect (solve (parse (string-reader example)))) [3749 11387]))
  (with-open [rdr (clojure.java.io/reader "input/day7")]
    (inspect (solve (parse rdr)))))
