(ns aoc2024.day13
  (:require [aoc2024.util :refer :all]))

(def example (multi-line
              "Button A: X+94, Y+34"
              "Button B: X+22, Y+67"
              "Prize: X=8400, Y=5400"
              ""
              "Button A: X+26, Y+66"
              "Button B: X+67, Y+21"
              "Prize: X=12748, Y=12176"
              ""
              "Button A: X+17, Y+86"
              "Button B: X+84, Y+37"
              "Prize: X=7870, Y=6450"
              ""
              "Button A: X+69, Y+23"
              "Button B: X+27, Y+71"
              "Prize: X=18641, Y=10279"))

(def pattern #"Button A: X\+([\d]+), Y\+([\d]+)\nButton B: X\+([\d]+), Y\+([\d]+)\nPrize: X=([\d]+), Y=([\d]+)")

(defn parse [str]
  (->> str
       (re-seq pattern)
       (map rest)
       (mapv #(mapv Integer/parseInt %))))

(defn solve-machine [machine]
  (let [[ax ay bx by px py] machine
        a [ax ay]
        b [bx by]
        p [px py]]

    (let [valid-solutions (map (fn [[ai bi]] (+ (* 3 ai) bi))
                               (filter (fn [[ai bi]] (v2= p (v2+ (v2*s a ai) (v2*s b bi))))
                                       (for [ai (range 100)
                                             bi (range 100)]
                                         [ai bi])))]
      (if (empty? valid-solutions)
        nil
        (reduce min valid-solutions)))))

(defn solve-machine2 [[ax ay bx by px py]]
  (let [da (- ax ay)
        db (- bx by)
        dp (- px py)]))
(defn solve [machines]
  [(sum  (filter some? (map solve-machine machines)))])

;;  ai * Ax + bi * Bx - X = 0
;;  ai * Ay + bi * By - Y = 0
;;  ai * Ax + bi * Bx - X = ai * Ay + bi * By - Y
;;  ai * Ax + bi * Bx - X - ai * Ay - bi * By + Y
;;  ai * Ax - ai * Ay + bi * Bx - bi * By = X - Y
;;  ai * (Ax - Ay) + bi * (Bx - By) = X - Y

(defn -main []
  (solve (parse example))
  (solve (parse (slurp "input/day13"))))
