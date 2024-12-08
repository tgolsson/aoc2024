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

(defmacro vx [a]
  `(get ~a 0))

(defmacro vy [a]
  `(get ~a 1))

(defmacro v2+ [a b]
  `[(+ (vx ~a) (vx ~b))
    (+ (vy ~a) (vy ~b))])

(defmacro v2- [a b]
  `[(- (vx ~a) (vx ~b))
    (- (vy ~a) (vy ~b))])

(defn v2-step
  "Computes the step between a and b as integer coordinates"
  [a b]
  (v2- b a))

(defn node-frequencies [grid]
  (difference (set (flatten grid)) #{\.}))

(defmacro get-at [grid pos]
  `(nth
    (nth ~grid (nth ~pos 1)) (nth ~pos 0)))

(defn grid-dimensions [grid]
  [(count (get grid 0)) (count grid)])

(defn inside-grid?
  ([dims position]
   (not (or (< (vx position) 0)
            (< (vy position) 0)
            (>= (vx position) (vx dims))
            (>= (vy position) (vy dims)))))
  ([grid]
   (let [dims (grid-dimensions grid)]
     (fn [position] (inside-grid? dims position)))))

(defn node-locations [grid freq]
  (for [y (range (count grid))
        x (range (count (nth grid 0)))
        :when (= (get-at grid [x y]) freq)]
    [x y]))

(defn antinodes-of [grid max-range a b]
  (let [step (v2-step a b)
        valid? (inside-grid? grid)]
    (inspect (concat (if (> max-range 1) [a b] []) (for [[_ pos]  (map vector (range max-range) (iterate #(v2- % step) (v2- a step)))
                           :while (valid? pos)]
                       pos)
                     (for [[_ pos] (map vector (range max-range) (iterate #(v2+ % step) (v2+ b step)))
                           :while (valid? pos)]
                       pos)))))

(defn gen-antinodes [grid locations max-range]
  (loop [current (nth locations 0)
         remainder (rest locations)
         res ()]
    (if (empty? remainder)
      (apply concat res)
      (recur (nth remainder 0)
             (rest remainder)
             (conj res (apply concat (map (partial antinodes-of grid max-range current) remainder)))))))

(defn parse [rdr]
  (->> (line-seq rdr)
       (mapv vec)))

(defn in-grid [grid antinodes]
  (loop [grid grid
         current (first antinodes)
         remainder (rest antinodes)]
    (if (empty? remainder)
      grid
      (recur (if (= (get-at grid current) \.)
               (update-in grid [(nth current 1) (nth current 0)] (fn [_] \#))
               grid)
             (nth remainder 0)
             (rest remainder)))))

(defn solve [grid]
  (let [frequencies (node-frequencies grid)
        freq-to-positions (map #(node-locations grid %) frequencies)]
    [(count (set (filter (inside-grid? grid) (apply concat (map #(gen-antinodes grid % 1) freq-to-positions)))))
     (count (set (filter (inside-grid? grid) (apply concat (map #(gen-antinodes grid % 100) freq-to-positions)))))]))

(defn -main []
  (assert (= (inspect (solve (parse (string-reader example)))) [14 34]))
  (with-open [rdr (clojure.java.io/reader "input/day8")]
    (inspect (solve (parse rdr)))))
