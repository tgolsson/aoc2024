(ns aoc2024.day20
  (:require [aoc2024.util :refer :all]
            [clojure.java.io]))

(def example (multi-line
              "###############"
              "#...#...#.....#"
              "#.#.#.#.#.###.#"
              "#S#...#.#.#...#"
              "#######.#.#.###"
              "#######.#.#...#"
              "#######.#.###.#"
              "###..E#...#...#"
              "###.#######.###"
              "#...###...#...#"
              "#.#####.#.###.#"
              "#.#...#.#.#...#"
              "#.#.#.#.#.#.###"
              "#...#...#...###"
              "###############"))

(defn parse [rdr]
  (mapv vec (line-seq rdr)))

(defn dijkstras [grid start end]
  (let [is-valid? (inside-grid? grid)
        grid (set-at (set-at grid end \.) start \.)]
    (loop [grid grid
           current [start 0]
           frontier []
           step 0]
      (if (= (first current) end)
        (set-at grid (first current) step)

        (let [[current cost] current
              new-frontier (for [offset [[0 1] [1 0] [0 -1] [-1 0]]
                                 :let [new-pos (v2+ current offset)]
                                 :when (and (is-valid? new-pos)
                                            (not= (get-at grid new-pos) \#)
                                            (or (= (get-at grid new-pos) \.)
                                                (< (+ cost 1) (get-at grid new-pos))))]
                             [new-pos (+ cost 1)])
              new-frontier (concat frontier new-frontier)]
          (recur (set-at grid current cost) (first new-frontier) (rest new-frontier) (inc step)))))))

(defn neighbors-in-range [grid pos distance]
  (let [is-valid? (inside-grid? grid)
        x (vx pos)
        y (vy pos)]
    (for [x (range (- x distance) (+ x distance 1))
          y (range (- y distance) (+ y distance 1))
          :let [new-pos [x y]]
          :when (and (<= (manhattan pos new-pos) distance)
                     (is-valid? new-pos)
                     (not= new-pos pos)
                     (not= (get-at grid new-pos) \#))]
      new-pos)))

(defn find-cheat [dijkstra-grid]
  (let [grid-dims (grid-dimensions dijkstra-grid)]
    (for [y (range (vy grid-dims))
          x (range (vx grid-dims))
          :let [pos [x y]
                neighbors (neighbors-in-range dijkstra-grid pos 20)
                here-cost (get-at dijkstra-grid pos)]
          :when (not= here-cost \#)]
      (filter #(>= % 100)
              (map (fn [[ps there-cost]] (- there-cost here-cost ps))
                   (map (fn [end] [(manhattan pos end) (get-at dijkstra-grid end)]) neighbors))))))

(defn solve [rdr]
  (let [grid (parse rdr)
        start (first (grid-where grid \S))
        end (first (grid-where grid \E))]
    (frequencies (flatten (find-cheat (dijkstras grid start end))))))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input/day20")]
    (sum (map second
              (solve rdr)))))
