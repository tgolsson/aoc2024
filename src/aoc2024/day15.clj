(ns aoc2024.day15
  (:require [aoc2024.util :refer :all]
            [clojure.string]
            [clojure.java.io]))

(def example (multi-line
              "##########"
              "#..O..O.O#"
              "#......O.#"
              "#.OO..O.O#"
              "#..O@..O.#"
              "#O#..O...#"
              "#O..O..O.#"
              "#.OO.O.OO#"
              "#....O...#"
              "##########"
              ""
              "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
              "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
              "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
              "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
              "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
              "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
              ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
              "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
              "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
              "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"))

(defn scale-map [grid]
  (mapv vec (for [row grid]
              (flatten (for [c row]
                         (case c
                           \# [\# \#]
                           \. [\. \.]
                           \O [\[ \]]
                           \@ [\@ \.]))))))

(defn direction-vector [direction]
  (case direction
    \^ [0 -1]
    \> [1 0]
    \v [0 1]
    \< [-1 0]))

(defn parse [rdr]
  (let [lines (line-seq rdr)
        [first second] (split-with #(not= % "") lines)]
    [(->> first
          (mapv vec))
     (->> (rest second)
          (apply concat)
          vec)]))

(defn get-starting-position [grid]
  (first (grid-where grid \@)))

(defn try-move [grid position direction]
  (let [dir (direction-vector direction)
        next-p (v2+ dir position)]
    (case (get-at grid next-p)
      \# false
      \. true
      \O (try-move grid next-p direction)
      \[ (if (or (= direction \>) (= direction \<))
           (try-move grid next-p direction)
           (and (try-move grid next-p direction) (try-move grid (v2+ next-p [1 0]) direction)))
      \] (if (or (= direction \>) (= direction \<))
           (try-move grid next-p direction)
           (and (try-move grid next-p direction) (try-move grid (v2+ next-p [-1 0]) direction))))))

(defn exec-move [grid position direction]
  (let [dir (direction-vector direction)
        next-p (v2+ dir position)
        [nx ny] next-p
        [px py] position]

    (-> (case (get-at grid next-p)
          \. grid
          \O (exec-move grid next-p direction)
          \[ (if (or (= direction \>) (= direction \<))
               (exec-move grid next-p direction)
               (exec-move (exec-move grid (v2+ next-p [1 0]) direction) next-p direction))
          \] (if (or (= direction \>) (= direction \<))
               (exec-move grid next-p direction)
               (exec-move (exec-move grid (v2+ next-p [-1 0]) direction) next-p direction)))
        (update-in [ny nx] (fn [_] (get-at grid position)))
        (update-in [py px] (fn [_] \.)))))

(defn do-move [grid position command]
  (if (try-move grid position command)
    [(exec-move grid position command) (v2+ position (direction-vector command))]
    [grid position]))

(defn execute [grid instructions]
  (let [robot-position (get-starting-position grid)
        grid (update-in grid [(second robot-position) (first robot-position)] (fn [_] \.))]
    (loop [command (first instructions)
           queue (rest instructions)
           robot-position robot-position
           grid grid]
      (if (empty? queue)
        grid
        (let [[new-grid new-pos] (do-move grid robot-position command)]
          (recur (first queue) (rest queue) new-pos new-grid))))))

(defn gps-score [grid]
  (let [dims (grid-dimensions grid)]
    (sum
     (for [y (range (vy dims))
           x (range (vx dims))
           :let [v (get-at grid [x y])]
           :when (or (= v \[) (= v \O))]
       (+ x (* 100 y))))))

(defn solve [grid instructions]
  [(gps-score (execute grid instructions))
   (gps-score (execute (scale-map grid) instructions))])

(defn -main []
  (assert (= (inspect (apply solve (parse (string-reader example)))) [10092 9021]))

  (with-open [rdr (clojure.java.io/reader "input/day15")]
    (apply  solve (parse rdr))))
