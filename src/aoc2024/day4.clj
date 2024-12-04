(ns aoc2024.day4
  (:require [clojure.java.io]
            [clojure.math]
            [clojure.core.match :refer [match]]
            [aoc2024.util :refer :all]))

(def example
  (multi-line
   "MMMSXXMASM"
   "MSAMXMSMSA"
   "AMXSXMAAMM"
   "MSAMASMSMX"
   "XMASAMXAMM"
   "XXAMMXXAMA"
   "SMSMSASXSS"
   "SAXAMASAAA"
   "MAMMMXMMMM"
   "MXMXAXMASX"))

(defn is-xmas [v]
  (or (= v [\X \M \A \S]) (= v [\S \A \M \X])))

(defn convolve [[k1 k2] kernel coll & {:keys [stride] :or {stride 1}}]
  (->> coll
       (partition k1 stride)
       (mapv (fn [%] (zip (mapv #(mapv vec (partition k2 stride %)) %))))
       (mapv #(map kernel %))))

(defn solve [reader]
  (let [listified (->> reader
                       line-seq
                       (map char-array)
                       (map #(into [] %)))]

    [(->> [(convolve [4 1] #(if (is-xmas (into [] (flatten %))) 1 0) listified)
           (convolve [1 4] #(if (is-xmas (into [] (flatten %))) 1 0) listified)
           (convolve [4 4] (fn [[a b c d]]
                             (match [a b c d]
                               [[\X _ _ \S] [_ \M \A _] [_ \M \A _] [\X _ _ \S]] 2
                               [[\S _ _ \S] [_ \A \A _] [_ \M \M _] [\X _ _ \X]] 2
                               [[\S _ _ \X] [_ \A \M _] [_ \A \M _] [\S _ _ \X]] 2
                               [[\X _ _ \X] [_ \M \M _] [_ \A \A _] [\S _ _ \S]] 2
                               [[\X _ _ _] [_ \M _ _] [_ _ \A _] [_ _ _ \S]] 1
                               [[\S _ _ _] [_ \A _ _] [_ _ \M _] [_ _ _ \X]] 1
                               [[_ _ _ \X] [_ _ \M _] [_ \A _ _] [\S _ _ _]] 1
                               [[_ _ _ \S] [_ _ \A _] [_ \M _ _] [\X _ _ _]] 1
                               :else 0)) listified)]
          flatten
          sum)
     (->> listified
          (convolve [3 3] (fn [[a b c]]
                            (match [a b c]
                              [[\M _ \S]  [_ \A _] [\M _ \S]] 1
                              [[\S _ \S]  [_ \A _] [\M _ \M]] 1
                              [[\S _ \M]  [_ \A _] [\S _ \M]] 1
                              [[\M _ \M]  [_ \A _] [\S _ \S]] 1
                              :else 0)))
          flatten
          sum)]))

(defn -main []
  (assert (= (inspect (solve (string-reader example))) [18 9]))

  (with-open [rdr (clojure.java.io/reader "input/day4")]
    (println (solve rdr))))
