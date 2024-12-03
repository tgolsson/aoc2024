(ns aoc2024.day2
  (:require [aoc2024.util :refer :all]))

(def example "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn parse-with-markers [s]
  (let [pattern #"(do\(\)|don't\(\)|mul\((\d+),\s*(\d+)\))"]
    (loop [matches (re-seq pattern s)
           enabled? true
           result []]
      (if (empty? matches)
        result
        (let [[full-match marker x y] (first matches)
              rest-matches (rest matches)]
          (cond
            (= marker "do()") (recur rest-matches true result)
            (= marker "don't()") (recur rest-matches false result)
            (and x y) (recur rest-matches enabled?
                             (conj result [(Integer/parseInt x) (Integer/parseInt y) enabled?]))
            :else (recur rest-matches enabled? result)))))))

(defn solve [pairs]
  [(->> pairs
        (map (fn [[a b ]] (* a b)))
        (reduce +))

   (->> pairs
        (map (fn [[a b enabled ]] [a b (if enabled 1 0)]))
        (map #(apply * %))
        (reduce +))])

(defn -main []
  (assert (= (inspect (solve (parse-with-markers example))) [161 48]))
  (println (solve (parse-with-markers (slurp "input/day3")))))
