(ns aoc2024.day16
  (:require [aoc2024.util :refer :all]
            [clojure.java.io]
            [clojure.data.priority-map :refer [priority-map-keyfn]]
            [clojure.math]))

(def example (multi-line
              "###############"
              "#.......#....E#"
              "#.#.###.#.###.#"
              "#.....#.#...#.#"
              "#.###.#####.#.#"
              "#.#.#.......#.#"
              "#.#.#####.###.#"
              "#...........#.#"
              "###.#.#####.#.#"
              "#...#.....#.#.#"
              "#.#.#.###.#.#.#"
              "#.....#...#.#.#"
              "#.###.#.#.#.#.#"
              "#S..#.....#...#"
              "###############"))

(def example2 (multi-line
               "#################"
               "#...#...#...#..E#"
               "#.#.#.#.#.#.#.#.#"
               "#.#.#.#...#...#.#"
               "#.#.#.#.###.#.#.#"
               "#...#.#.#.....#.#"
               "#.#.#.#.#.#####.#"
               "#.#...#.#.#.....#"
               "#.#.#####.#.###.#"
               "#.#.#.......#...#"
               "#.#.###.#####.###"
               "#.#.#...#.....#.#"
               "#.#.#.#####.###.#"
               "#.#.#.........#.#"
               "#.#.#.#########.#"
               "#S#.............#"
               "#################"))

(defn parse [rdr]
  (mapv vec (line-seq rdr)))

(defn walk [grid pos dir valid?]
  (loop [pos pos next-pos (v2+ pos dir)]
    (if (or (not (valid? next-pos))
            (= (get-at grid next-pos) \#))
      pos
      (let [neighbours (filter #(and (not (= pos %)) (not (= (v2+ next-pos dir) %))) (neighbours-with-value grid next-pos \. valid?))]
        (if (> (count neighbours) 0)
          next-pos
          (recur next-pos (v2+ next-pos dir)))))))

(defn print-jump-map [jump-map]
  (let [[w h]  (grid-dimensions jump-map)
        print-w (* 7 w)
        print-h (* 4 h)]
    (doseq [y (range (+ print-h 1))]
      (doseq [x (range (+ print-w 1))]
        (let [box-y (mod y 4)
              box-x (mod x 7)]
          (cond
            (and (= 0 y) (= 0 x)) (print "┌")
            (and (= 0 y) (= print-w x)) (print "┐")
            (and (= print-h y) (= 0 x)) (print "└")
            (and (= print-h y) (= print-w x)) (println "┘")
            (and (= 0 box-y) (= 0 x)) (print "├")
            (and (= print-h y) (= 0 box-x)) (print "┴")
            (and (= 0 box-y) (= print-w x)) (print "┤")
            (and (= box-x 0) (= box-y 0)) (print "┼")

            (= 0 box-y) (print "─")
            (= 0 box-x) (print "│")
            (and (= 0 box-y) (or (= 0 box-x) (= 6 box-x))) (print "─")
            :else (when (= box-x 1) (let [jump (get-in jump-map [(quot y 4) (quot x 7)])]
                                      (case box-y
                                        1 (printf "  %2d  " (first jump))
                                        2 (printf "%2d  %2d"  (last jump) (second jump))
                                        3 (printf "  %2d  " (nth jump 2))))))
          (when (= x print-w) (println)))))))

;; go through the grid and generate a Jump map
(defn make-jump-map [grid]
  (let [[w h] (grid-dimensions grid)
        valid? (inside-grid? grid)]
    (loop [jump-map (vec (repeat h (vec (repeat w [0 0 0 0])))) ;; up right down left
           x 0 y 0]
      (cond
        (and (= w (inc x)) (= h (inc y))) jump-map
        (>= y h) jump-map
        (>= x w) (recur jump-map 0 (inc y))
        (= (get-at grid [x y]) \#) (recur jump-map (inc x) y)
        :else
        (recur (assoc-in jump-map [y x] (vec (map (fn [[dx dy]]
                                                    (let [maybe-branch (walk grid [x y] [dx dy] valid?)]
                                                      (if (= maybe-branch [x y])
                                                        0
                                                        (manhattan [x y] maybe-branch))))
                                                  [[0 -1] [1 0] [0 1] [-1 0]])))
               (inc x) y)))))

(defn turn-cost [dir new-dir]
  (cond (v2= dir new-dir) 0
        (v2= (v2*s dir -1) new-dir) 2000
        :else 1000))

(defn heuristic [pos end dir new-dir]
  (+ (manhattan pos end) (turn-cost dir new-dir)))

(defrecord Node [position direction g h visited previous])

(defn search-iterative [jump-map start end]
  (loop [node  (Node. start [1 0] 0 (heuristic start end [1 0] [1 0]) #{} [])
         nodes (priority-map-keyfn (fn [node] (+ (:g node) (:h node))))
         expanded {}
         solutions ()
         solution 10000000
         step 0]

    (when (= 0 (mod step 10000))
      (printf "%10d: %s %10d %10d\n" step (str (:position node)) (+ (:g node) (:h node)) (count nodes)))

    (cond
      ;; no more nodes
      (not (some? node)) solutions
      ;; all remaining nodes are worse than the best solution
      (and (seq  solutions) (< (:g (peek solutions))  (:g node))) solutions
      ;; we have a solution but more nodes to explore
      (and (v2= (:position node) end) (empty? solutions)) (recur (second (first nodes)) (pop nodes) expanded (conj solutions node) (:g node) (inc step))
        ;; we have a solution and no more nodes to explore
      (and (v2= (:position node) end) (empty? nodes)) solutions
        ;; we have a solution and more nodes to explore
      (v2= (:position node) end) (recur (second (first nodes)) (pop nodes) expanded (conj solutions node) solution (inc step))
      ;; we have a node to explore
      :else
      (let [[up right down left] (get-at jump-map (:position node))
            children (for [[new-dir distance] [[[0 -1] up] [[1 0] right] [[0 1] down] [[-1 0] left]]
                           :when (not (= distance 0))
                           :let [new-pos (v2+ (:position node) (v2*s new-dir distance))
                                 new-node (Node. new-pos new-dir (+ (:g node) distance (turn-cost (:direction node) new-dir))
                                                 (heuristic new-pos end (:direction node) new-dir)
                                                 (conj (:visited node) (:position node))
                                                 (conj (:previous node) (:position node)))]
                           :when (not (contains? (:visited node) new-pos))
                           :when (or (not (contains? expanded [new-pos new-dir]))
                                     (<= (:g new-node) (get expanded [new-pos new-dir])))]
                       new-node)

            all-nodes (into nodes (map (fn [child] [child child]) children))
            new-expanded (assoc expanded [(:position node) (:direction node)] (:g node))]
        (if (seq all-nodes)
          (recur (second (peek all-nodes)) (pop all-nodes) new-expanded solutions solution (inc step))
          solutions)))))

;; convert waypoints to a path with all the steps
(defn expand-waypoints [a b]
  (let [dx (clojure.math/signum (- (vx b) (vx a)))
        dy (clojure.math/signum (- (vy b) (vy a)))]

    (cond
      (= dx 1.0) (for [x (range  (vx a) (inc (vx b)))] [x (vy a)])
      (= dx -1.0) (reverse (for [x (range (vx b) (inc (vx a)))] [x (vy a)]))
      (= dy 1.0) (for [y (range (vy a) (inc (vy b)))] [(vx a) y])
      (= dy -1.0) (reverse (for [y (range  (vy b) (inc (vy a)))] [(vx a) y])))))

(defn expand-path [path]
  (apply concat (for [[a b] (partition 2 1 path)]
                  (expand-waypoints a b))))

(defn search [grid]
  (let [start (first (grid-where grid \S))
        end (first (grid-where grid \E))
        jump-map (make-jump-map (-> grid
                                    (assoc-in [(vy end) (vx end)] \.)
                                    (assoc-in  [(vy start) (vx start)] \.)))
        paths (search-iterative jump-map start  end)]
    [(:g (first paths))
     (count (set (apply concat (map (comp expand-path #(conj (:previous %) (:position %))) paths))))]))

(defn -main []
  (assert (= (inspect (search (parse (string-reader example)))) [7036 45]))
  (assert (= (inspect (search (parse (string-reader example2)))) [11048 64]))
  (with-open [rdr (clojure.java.io/reader "input/day16")]
    (let [grid (parse rdr)]
      (println (search grid)))))
