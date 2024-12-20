(ns aoc2024.day19
  (:require [aoc2024.util :refer :all]
            [clojure.string]
            [clojure.java.io]))

(def example (multi-line
              "r, wr, b, g, bwu, rb, gb, br"
              ""
              "brwrr"
              "bggr"
              "gbbr"
              "rrbgbr"
              "ubwu"
              "bwurrg"
              "brgr"
              "bbrgwb"))

(def final-map (atom {}))

(defn parse [rdr]
  (let [[towels patterns]  (split-with #(not (= % "")) (line-seq rdr))]
    [(mapv vec (clojure.string/split (first towels) #", "))
     (mapv vec (rest patterns))]))

(defn gen-recursion [pattern towels [i t]]
  [(vec (drop (count t) pattern))
   towels])

(defn solve-pattern [pattern towels]
  (let [indexed-candidates (keep-indexed #(when (is-prefix? %2 pattern) [%1 %2]) towels)]
    (cond
      (contains? @final-map pattern) (get @final-map pattern)
      (empty? pattern) 1
      (empty? indexed-candidates) 0
      :else (let [count (sum (map #(apply solve-pattern %) (map #(gen-recursion pattern towels %1) indexed-candidates)))]
              (swap! final-map assoc pattern count)
              count))))

(defn solve [rdr]
  (let [[towels patterns] (parse rdr)
        solutions  (filter #(not (zero? %)) (map #(solve-pattern % towels) patterns))]
    [(count solutions)
     (sum solutions)]))

(defn -main []
  (swap! final-map {})
  (assert (= (inspect (solve (string-reader example))) [6 16]))
  (swap! final-map {})
  (println (with-open [rdr (clojure.java.io/reader "input/day19")]
             (solve rdr)))
  (shutdown-agents))
