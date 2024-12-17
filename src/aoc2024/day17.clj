(ns aoc2024.day17
  (:require [aoc2024.util :refer :all]
            [clojure.string]
            [clojure.java.io]))

(defn parse [rdr]
  (let [lines (line-seq rdr)
        [p1 p2] (split-with #(not= % "") lines)]
    [(->> p1
          (mapv #(re-find #"[\d]+$" %))
          (mapv Integer/parseInt))
     (as-> (first (rest p2)) $
       (subs $ (count "Program: "))
       (clojure.string/split $ #",")
       (mapv Integer/parseInt $))]))

(defn resolve-operand [regs op]
  (case op
    0 0
    1 1
    2 2
    3 3
    4 (first regs)
    5 (second regs)
    6 (last regs)
    7 nil))

(defn execute [r program]
  (loop [ip 0
         regs r
         output []]
    ;; (println ip regs output)
    ;; (Thread/sleep 10)
    (if (>= ip (count program))
      output
      (let [insn (nth program ip)
            op (nth program (inc ip))
            op-v (resolve-operand regs op)]
        ;; (println insn op op-v)
        (case insn
          ;; adv
          0 (recur (+ ip 2) (update-in regs [0] #(bit-shift-right % op-v)) output)
          ;; bxl
          1 (recur (+ ip 2) (update-in regs [1] #(bit-xor % op)) output)
          ;; bst
          2 (recur (+ ip 2) (update-in regs [1] (fn [_] (mod op-v 8))) output)
          ;; jnz
          3 (if (zero? (first regs))
              (recur (+ ip 2) regs output)
              (recur op regs output))
          ;; bxc
          4 (recur (+ ip 2) (update-in regs [1] #(bit-xor % (last regs))) output)
          ;; out
          5 (recur (+ ip 2) regs (conj output (mod op-v 8)))
          ;; bdv
          6 (recur (+ ip 2) (assoc-in regs [1] (bit-shift-right (first regs) op-v)) output)
          ;; cdv
          7 (recur (+ ip 2) (assoc-in regs [2] (bit-shift-right (first regs) op-v)) output))))))

(defn is-prefix? [prefix coll]
  (and (<= (count prefix) (count coll))
       (every? (fn [[a b]] (= a b)) (zip [prefix coll]))))

(defn search-a [program s]

  (first (filter some? (for [a (range s (+ s 8))
                             :let [result (execute [a 0 0] program)]]
                         (cond
                           (= program result) a
                           (is-prefix? (reverse result) (reverse program)) (search-a program (* 8 a))
                           :else nil)))))
(defn solve [rdr]
  (let [[regs prog] (parse rdr)]
    [(clojure.string/join \, (execute regs prog))
     (search-a prog 1)]))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input/day17")]
    (println (solve rdr)))
  (shutdown-agents))
