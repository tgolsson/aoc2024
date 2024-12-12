(ns aoc2024.day10
  (:require [aoc2024.util :refer :all]
            [clojure.java.io]))

(def example
  (multi-line
   "89010123"
   "78121874"
   "87430965"
   "96549874"
   "45678903"
   "32019012"
   "01329801"
   "10456732"))

(defn parse [rdr]
  (->> rdr
       line-seq
       (mapv #(mapv (comp Integer/parseInt str) %))))

(defn trailheads [grid] (grid-where grid 0))

(defn flood-fill-from [grid valid? location]
  (loop [visited #{}
         frontier (seq [location])]
    (if (empty? frontier)
      visited
      (let [front (first frontier)
            valid-neighbours (neighbours-with-value grid front (+ (get-at grid front) 1) valid?)]
        (recur (conj visited front) (concat (rest frontier) valid-neighbours))))))

(defn dfs-from [grid visited valid? location]
  (if (= (get-at grid location) 9)
    1
    (let [valid-neighbours (filter #(not (contains? visited %)) (neighbours-with-value grid location (+ (get-at grid location) 1) valid?))]
      (or
       (and (empty? valid-neighbours)    0)
       (map #(dfs-from grid (conj visited location) valid? %) valid-neighbours)))))

(defn solve [grid]
  (let [starts (trailheads grid)
        valid? (inside-grid? grid)]
    [(->> starts
          (map (partial flood-fill-from grid valid?))
          (map (fn [visited] (filter #(= (get-at grid %) 9) visited)))
          (apply concat)
          count)
     (->> starts
          (map (partial dfs-from grid #{} valid?))
          flatten
          sum)]))

(defn -main []
  (inspect (solve (parse (string-reader example))))

  (with-open [rdr (clojure.java.io/reader "input/day10")]
    (inspect (solve (parse rdr)))))
