(ns aoc2024.day1
  (:require [clojure.java.io]
            [clojure.string :as str]
            [aoc2024.util :refer :all]))

(def example "3 4\n4 3\n2 5\n1 3\n3 9\n3 3")

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

(defn -main []
  (assert (= (solve (string-reader example)) [11 31]))

  (with-open [rdr (clojure.java.io/reader "input/day1")]
    (println (solve rdr))))
