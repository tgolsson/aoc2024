(ns aoc2024.day2
  (:require [clojure.java.io]
            [clojure.math]
            [clojure.string :as str]
            [aoc2024.util :refer :all]))

(def example
  (multi-line
   "7 6 4 2 1"
   "1 2 7 8 9"
   "9 7 6 2 1"
   "1 3 2 4 5"
   "8 6 4 4 1"
   "1 3 6 7 9"))

(defn parse-line [line]
  (as-> line $
    (str/split $ #" +")
    (map Integer/parseInt $)))

(defn parse [input-reader]
  (->> input-reader
       line-seq
       (map parse-line)))

(defn line-safe [line]
  (let [d (->> line
               (partition 2 1)
               (map #(apply - %)))
        sign (clojure.math/signum (first d))]

    (every? #(and (<= 1 (abs %) 3)
                  (== sign (clojure.math/signum %))) d)))

(defn dampen [line]
  (some #(line-safe (vec-remove % (into [] line))) (range (count line))))

(defn solve [input-reader]
  (let [report (parse input-reader)]
    [(->> report
          (filter line-safe)
          count)
     (->> report
          (filter #(or (line-safe %) (dampen %)))
          count)]))

(defn -main []
  (assert (= (inspect (solve (string-reader example))) [2 4]))

  (with-open [rdr (clojure.java.io/reader "input/day2")]
    (println (solve rdr))))
