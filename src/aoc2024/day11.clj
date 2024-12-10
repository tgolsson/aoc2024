(ns aoc2024.day11
  (:require [aoc2024.util :refer :all]
            [clojure.string]))

(def example "125 17")
(def input "17639 47 3858 0 470624 9467423 5 188")
(def final-map (atom {}))

(defn parse [string]
  (map Long/parseLong (clojure.string/split string #" +")))

(defn split [n]
  (let [c (/ (num-digits n) 2)
        divisor (exp 10 c)
        upper (long (/ n divisor))]
    [upper (- n (* upper divisor))]))

(defn blink [stone depth]
  (cond
    (= depth 0) 1
    (contains? @final-map [stone depth]) (get @final-map [stone depth])
    :else (let [[upper lower] (split stone)
                res (cond
                      (= stone 0) (blink (inc stone) (dec depth))
                      (even? (num-digits stone)) (+ (blink upper (dec depth)) (blink lower (dec depth)))
                      :else (blink (* stone 2024) (dec depth)))]
            (swap! final-map assoc [stone depth] res)
            res)))

(defn solve [stones]
  [(sum (map #(blink % 25) stones))
   (sum (map #(blink % 75) stones))])

(defn -main []
  (swap! final-map {})
  (assert (= (inspect (solve (parse example))) [55312 65601038650482]))
  (println (solve (parse input))))
