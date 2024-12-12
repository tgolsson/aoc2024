(ns aoc2024.day12
  (:require [aoc2024.util :refer :all]
            [clojure.set]
            [clojure.java.io]))

(def example (multi-line
              "RRRRIICCFF"
              "RRRRIICCCF"
              "VVRRRCCFFF"
              "VVRCCCJFFF"
              "VVVVCJJCFE"
              "VVIVCCJJEE"
              "VVIIICJJEE"
              "MIIIIIJJEE"
              "MIIISIJEEE"
              "MMMISSJEEE"))

(defn parse [rdr]
  (mapv vec (line-seq rdr)))

(defn fence-count [grid valid? coord]
  (let [value (get-at grid coord)]
    (->> (neighbours-of coord)
         (filter #(or (not (valid? %)) (not (= (get-at grid %) value))))
         count)))

(defn flood-fill-from [grid valid? location]
  (let [plant (get-at grid location)]
    (loop [visited #{}
           expanded #{location}
           frontier (seq [location])]
      (if (empty? frontier)
        visited
        (let [front (first frontier)
              valid-neighbours (neighbours-with-value grid front plant valid?)]
          (recur (conj visited front)
                 (clojure.set/union expanded (set valid-neighbours))
                 (concat (rest frontier) (filter #(not (contains? expanded %)) valid-neighbours))))))))

(defn gen-regions [grid]
  (let [dims (grid-dimensions grid)
        valid? (inside-grid? grid)]
    (loop [regions ()
           frontier (set (linspace dims))]
      (if (empty? frontier)
        (mapv vec regions)
        (let [visited (flood-fill-from grid valid? (first frontier))]
          (recur (conj regions visited) (clojure.set/difference frontier visited)))))))

(defn is-inner-corner? [c n1 n2 d]
  (and (= c n1 n2) (not (= c d))))

(defn is-outer-corner? [c n1 n2]
  (and (not (= c n1)) (not (= c n2))))

(defn corners-in [[[d1 n1 d2] [n2 value n3] [d3 n4 d4]]]
  (+ (if (is-inner-corner? value n1 n2 d1) 1 0)
     (if (is-inner-corner? value n1 n3 d2) 1 0)
     (if (is-inner-corner? value n4 n2 d3) 1 0)
     (if (is-inner-corner? value n4 n3 d4) 1 0)
     (if (is-outer-corner? value n1 n2) 1 0)
     (if (is-outer-corner? value n1 n3) 1 0)
     (if (is-outer-corner? value n4 n2) 1 0)
     (if (is-outer-corner? value n4 n3) 1 0)))

(defn kernel [grid is-valid? location]
  (mapv vec (for [y (range -1 2)]
              (for [x (range -1 2)]
                (get-at-safe grid is-valid? (v2+ location [x y]))))))

(defn count-corners [grid is-valid? region]
  (* (count region) (sum (map #(corners-in (kernel grid is-valid? %)) region))))

(defn region-cost [grid is-valid? region]
  (* (sum (map #(fence-count grid is-valid? %) region)) (count region)))

(defn solve [grid]
  (let [is-valid? (inside-grid? grid)
        regions (gen-regions grid)]
    [(sum (map #(region-cost grid is-valid? %) regions))
     (sum (map #(count-corners grid is-valid? %) regions))]))

(defn -main []
  (assert (= (inspect (solve (parse (string-reader example)))) [1930 1206]))
  (with-open [rdr (clojure.java.io/reader "input/day12")]
    (inspect (solve (parse rdr)))))
