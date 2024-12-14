(ns aoc2024.day14
  (:require [aoc2024.util :refer :all]
            [aoc2024.day14.part2 :as drawing]
            [quil.applet :as qa]
            [quil.core :as q]
            [quil.middleware :as qm]))

(def example (multi-line
              "p=0,4 v=3,-3"
              "p=6,3 v=-1,-3"
              "p=10,3 v=-1,2"
              "p=2,0 v=2,-1"
              "p=0,0 v=1,3"
              "p=3,0 v=-2,-2"
              "p=7,6 v=-1,-3"
              "p=3,0 v=-1,-2"
              "p=9,3 v=2,3"
              "p=7,3 v=-1,2"
              "p=2,4 v=2,-3"
              "p=9,5 v=-3,-3"))

(defn parse [str]
  (map #(partition 2 2 %) (map #(map Integer/parseInt %) (map rest (re-seq #"p=([\d]+),([\d]+) v=([-\d]+),([-\d]+)" str)))))

(defn step-robot [[[px py] [vx vy]] w h steps]
  [(mod (+ px (* vx steps)) w)
   (mod (+ py (* vy steps)) h)])

(defn quad-of [[x y] w h]
  (let [hw (int (/ w 2))
        hh (int (/ h 2))]
    (cond
      (and (< x hw) (< y hh)) 0
      (and (< x hw) (> y hh)) 1
      (and (> x hw) (< y hh)) 2
      (and (> x hw) (> y hh)) 3
      :else nil)))

(defn solve [robots w h steps]
  (->> robots
       (map #(step-robot % w h steps))
       (map #(quad-of % w h))
       (reduce (fn [reduction quad] (if (some? quad) (update-in reduction [quad] inc) reduction)) {0 0 1 0 2 0 3 0})
       vals
       (apply *)))

(q/defsketch example
  :title "An example quil sketch"
  :size [1010 1030]
  ; setup function called only once, during sketch initialization.
  :setup #(drawing/setup (parse (slurp "input/day14")))
  ; update-state is called on each iteration before draw-state.
  :update drawing/update-state
  :draw drawing/draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [qm/fun-mode])

(comment
  (use 'aoc2024.day14.part2 :reload)
  (qa/with-applet example (q/no-loop))
  (qa/with-applet example (q/start-loop))
  (qa/with-applet example (q/random 10)))

(defn -main []
  (assert (= (inspect (solve (parse example) 11 7 100)) 12))
  (inspect (solve (parse (slurp "input/day14")) 101 103 100)))
