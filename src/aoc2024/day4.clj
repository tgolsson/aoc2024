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

(defn transpose [& lists]
  (map #(into [] %) (apply map list lists)))

(defn is-xmas [v]
  (or (= v [\X \M \A \S]) (= v [\S \A \M \X])))

(defn convolve [[k1 k2] kernel coll & {:keys [stride] :or {stride 1}}]
  (->> coll
       (partition k1 stride nil)
       (map (fn [%] (zip (map #(partition k2 stride nil %) %))))
       (map #(map kernel %))))

(defn solve [reader]
  (let [listified (->> reader
                       line-seq
                       (map char-array)
                       (map #(into [] %)))]

    [(+ (->> (concat listified (apply transpose listified))
             (map #(->> %
                        (partition 4 1 [0])
                        (filter is-xmas)
                        count))
             (reduce +))
        (->> listified
             (convolve [4  4] #(if (> (count %) 3) (match [(first %) (second %) (nth % 2) (nth % 3)]
                                                     [([\X _ _ \S] :seq) ([_ \M \A _] :seq) ([_ \M \A _] :seq)  ([\X _ _ \S] :seq)] 2
                                                     [([\S _ _ \S] :seq) ([_ \A \A _] :seq) ([_ \M \M _] :seq)  ([\X _ _ \X] :seq)] 2
                                                     [([\S _ _ \X] :seq) ([_ \A \M _] :seq) ([_ \A \M _] :seq)  ([\S _ _ \X] :seq)] 2
                                                     [([\X _ _ \X] :seq) ([_ \M \M _] :seq) ([_ \A \A _] :seq)  ([\S _ _ \S] :seq)] 2
                                                     [([\X _ _ _] :seq) ([_ \M _ _] :seq) ([_ _ \A _] :seq)  ([_ _ _ \S] :seq)] 1
                                                     [([\S _ _ _] :seq) ([_ \A _ _] :seq) ([_ _ \M _] :seq)  ([_ _ _ \X] :seq)] 1
                                                     [([_ _ _ \X] :seq) ([_ _ \M _] :seq) ([_ \A _ _] :seq)  ([\S _ _ _] :seq)] 1
                                                     [([_ _ _ \S] :seq) ([_ _ \A _] :seq) ([_ \M _ _] :seq)  ([\X _ _ _] :seq)] 1
                                                     :else 0) 0))
             flatten
             (reduce +)))
     (->> listified
          (convolve [3 3] #(if (> (count %) 2)
                             (match [(first %) (second %) (nth % 2)]
                               [([\M _ \S] :seq) ([_ \A _] :seq) ([\M _ \S] :seq)] 1
                               [([\S _ \S] :seq) ([_ \A _] :seq) ([\M _ \M] :seq)] 1
                               [([\S _ \M] :seq) ([_ \A _] :seq) ([\S _ \M] :seq)] 1
                               [([\M _ \M] :seq) ([_ \A _] :seq) ([\S _ \S] :seq)] 1
                               :else 0)
                             0))
          flatten
          (reduce +))]))

(defn -main []
  (assert (= (inspect (solve (string-reader example))) [18 9]))

  (with-open [rdr (clojure.java.io/reader "input/day4")]
    (println (solve rdr))))
