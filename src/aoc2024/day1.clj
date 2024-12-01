(ns aoc2024.day1
  (:import  (java.io BufferedReader StringReader))
  (:require [clojure.string :as str]))

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

(defn read-string [str]
  (BufferedReader. (StringReader. str)))

(defn parse [input-reader]
  (->> input-reader
       line-seq
       (map #(map Integer/parseInt (str/split % #" +")))
       unzip))

(defn solve [input-reader]
  (let [[left right] (parse input-reader)]
    [(->> (zip (map sort [left right]))
          (map #(abs (- (first %) (second %))))
          (reduce +))
     (->> left
          (map #(* % (count-occurrences % right)))
          (reduce +))]))

(def example "3 4\n4 3\n2 5\n1 3\n3 9\n3 3")
(defn -main [& args]
  (assert (= (solve (read-string example)) [11 31]))
  (with-open [rdr (clojure.java.io/reader "input/day1")]
    (println (solve rdr))))
