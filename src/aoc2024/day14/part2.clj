(ns aoc2024.day14.part2
  (:require [quil.core :as q]
            [clojure.math]))

(defn step-robot-once [[[px py] [vx vy]] steps]
  [[(mod (+ px (* vx steps)) 101)
    (mod (+ py (* vy steps)) 103)]
   [vx vy]])

(defn setup [robots]
  (q/frame-rate 1)
  {:robots (map #(step-robot-once % 7350) robots)
   :frame 7350
   :steps 1
   :scale 10})

(defn update-state [state]
  (-> state
      (update-in [:frame] #(+ (get state :steps) %))
      (update-in [:robots] #(map (fn [robot] (step-robot-once robot (get state :steps))) %))))

(defn draw-state [state]
  (q/background 255)
  (q/fill 0 0 0)
  ;; (let [steps (get state :steps)
  ;;       scale (get state :scale)]
  ;;   (doseq [steps (range steps)
  ;;           :let [ox (* 101 (mod steps scale))
  ;;                 oy (* 103 (int (/ steps scale)))
  ;;                 robots (map #(step-robot-once % steps) (get state :robots))]]
  ;;     (doseq [[[x y] _] robots]
  ;;       (q/rect (* scale (+ ox x)) (* scale (+ oy y))))
  (doseq [[[x y] _] (get state :robots)]
    (q/rect (* 10 x) (* 10 y) 10 10))
  (println (get state :frame)))

(comment
  ;; Evaluate these using the custom commands shortcut `ctrl+space t`
  (q/current-fill)
  (q/frame-rate 60)
  [(q/mouse-x) (q/mouse-y)])
