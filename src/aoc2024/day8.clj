(ns aoc2024.day8
  (:require [aoc2024.util :refer :all]
            [clojure.set :refer [difference]]
            [clojure.java.io]))

(def example
  (multi-line
   "............"
   "........0..."
   ".....0......"
   ".......0...."
   "....0......."
   "......A....."
   "............"
   "............"
   "........A..."
   ".........A.."
   "............"
   "............"))

(defn parse [rdr]
  (->> (line-seq rdr)
       (mapv vec)))

(defn node-frequencies [grid]
  (difference (set (flatten grid)) #{\.}))

(defn node-locations [grid freq]
  (for [y (range (count grid))
        x (range (count (nth grid 0)))
        :when (= (get-at grid [x y]) freq)]
    [x y]))

(defn trace [valid? max-range iter]
  (for [[_ pos] (map vector (range max-range) iter)
        :while (valid? pos)]
    pos))

(defn antinodes-of [grid max-range a b]
  (let [step (v2- b a)
        valid? (inside-grid? grid)]
    (concat (if (> max-range 1) [a b] [])
            (trace valid? max-range (iterate #(v2- % step) (v2- a step)))
            (trace valid? max-range (iterate #(v2+ % step) (v2+ b step))))))

(defn gen-antinodes [grid locations max-range]
  (loop [current (nth locations 0)
         remainder (rest locations)
         res ()]
    (if (empty? remainder)
      (apply concat res)
      (recur (nth remainder 0)
             (rest remainder)
             (conj res (apply concat (map (partial antinodes-of grid max-range current) remainder)))))))

(defn solve [grid]
  (let [frequencies (node-frequencies grid)
        freq-to-positions (map #(node-locations grid %) frequencies)]
    [(count (set (filter (inside-grid? grid) (apply concat (map #(gen-antinodes grid % 1) freq-to-positions)))))
     (count (set (filter (inside-grid? grid) (apply concat (map #(gen-antinodes grid % 50) freq-to-positions)))))]))

(defn -main []
  (assert (= (inspect (solve (parse (string-reader example)))) [14 34]))
  (with-open [rdr (clojure.java.io/reader "input/day8")]
    (inspect (solve (parse rdr)))))
