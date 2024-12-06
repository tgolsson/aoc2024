(ns aoc2024.day6
  (:require [aoc2024.util :refer :all]
            [clojure.string]
            [clojure.math]
            [clojure.java.io]
            [clojure.set]))

(def example
  (multi-line
   "....#....."
   ".........#"
   ".........."
   "..#......."
   ".......#.."
   ".........."
   ".#..^....."
   "........#."
   "#........."
   "......#..."))

(defn parse [rdr]
  (->> (line-seq rdr)
       (mapv (fn [line] (mapv (fn [char] (case char
                                           \. 0
                                           \# 1
                                           \^ 2)) line)))))

(defmacro get-at [grid pos]
  `(nth
    (nth ~grid (nth ~pos 1)) (nth ~pos 0)))

(defn get-starting-position [grid]
  (first (for [y (range (count grid))
               x (range (count (nth grid 0)))
               :when (= 2 (get-at grid [x y]))]
           [x y])))

(defn grid-dimensions [grid]
  [(count (get grid 0)) (count grid)])

(defmacro vx [a]
  `(get ~a 0))

(defmacro vy [a]
  `(get ~a 1))

(defmacro v2+ [a b]
  `[(+ (vx ~a) (vx ~b))
    (+ (vy ~a) (vy ~b))])

(defn next-direction [direction]
  (case direction
    :up :right
    :right :down
    :down :left
    :left :up))

(defn direction-vector [direction]
  (case direction
    :up [0 -1]
    :right [1 0]
    :down [0 1]
    :left [-1 0]))

(defn escaped-grid? [dims position]
  (or (< (vx position) 0)
      (< (vy position) 0)
      (>= (vx position) (vx dims))
      (>= (vy position) (vy dims))))

(defn walk-from [grid dims position direction]
  (let [direction (direction-vector direction)]
    (loop [here position
           next-position (v2+ position direction)
           visited [here]]
      (cond
        (escaped-grid? dims next-position) [next-position visited]
        (= (get-at grid next-position) 1) [here visited]
        :else (recur next-position (v2+ here direction) (conj visited here))))))

(defn walk [grid position]
  (let [dims (grid-dimensions grid)]
    (loop [visited ()
           position position
           direction :up]
      (let [[end visited-here] (walk-from grid dims position direction)]
        (if
         (escaped-grid? dims end) (clojure.set/difference
                                   (clojure.set/union (into #{} visited) (into #{} visited-here)) #{end})
         (recur (concat visited visited-here) end (next-direction direction)))))))

(defn with-obstacle [grid location]
  (update-in grid [(nth location 1) (nth location 0)] (fn [_] 1)))

(defn walk-from-no-track [grid dims position direction]
  (let [direction (direction-vector direction)]
    (loop [here position
           next-position (v2+ position direction)]
      (cond
        (escaped-grid? dims next-position) next-position
        (= (get-at grid next-position) 1) here
        :else (recur next-position (v2+ here direction))))))

(defn causes-loop? [grid start]
  (let [dims (grid-dimensions grid)]
    (loop [position start
           direction :up
           visited #{[position :up]}]
      (let [end (walk-from-no-track grid dims position direction)
            next-dir (next-direction direction)]
        (cond
          (escaped-grid? dims end) false
          (contains? visited [end next-dir]) true
          :else (recur end next-dir (conj visited [end next-dir])))))))

(defn solve [rdr]
  (let [grid (parse rdr)
        start (get-starting-position grid)
        visited (walk grid start)]
    [(count visited)
     (->> (clojure.set/difference visited (set [start]))
          (filter #(causes-loop? (with-obstacle grid %) start))
          count)]))

(defn -main []
  (assert (= (inspect (solve (string-reader example))) [41 6]))
  (with-open [rdr (clojure.java.io/reader "input/day6")]
    (inspect (solve rdr)))

  (shutdown-agents))
