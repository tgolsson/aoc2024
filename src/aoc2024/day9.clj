(ns aoc2024.day9
  (:require [aoc2024.util :refer :all]
            [clojure.string]))

(def example "2333133121414131402")

(defn parse [data]
  (let [parsed (map (comp Integer/parseInt str) (clojure.string/trim data))]
    (apply map list (partition 2 2 [0] parsed))))

(defn checksum [segments]
  (sum (map-indexed *  (apply concat (map #(repeat (nth % 2) (nth % 1)) segments)))))

(defn insert [v i e] (vec (concat (subvec v 0 i) [e] (subvec v i))))

(defn part-1 [disk]
  (loop [res []
         head 0
         tail (- (count disk) 1)
         disk disk]

    (cond
      (> head tail) res
      (= :u (nth (nth disk head) 0)) (recur (conj res (nth disk head)) (inc head) tail disk)
      (= :u (nth (nth disk tail) 0))
      (let [head-len (nth (nth disk head) 2)
            tail-len (nth (nth disk tail) 2)]

        (cond
          (= head-len tail-len) (recur (conj res (nth disk tail)) (inc head) (dec tail) disk)
          (> head-len tail-len) (recur (conj res (nth disk tail)) head (dec tail) (update-in disk [head 2] (fn [v] (- v tail-len))))
          (< head-len tail-len) (recur (conj res (update-in (nth disk tail) [2] (fn [v] head-len))) (inc head) tail (update-in disk [tail 2] (fn [v] (- v head-len))))))
      :else (recur res head (dec tail) disk))))

(defn part-2 [disk]
  (loop [tail (- (count disk) 1)
         disk disk]
    (cond
      (= tail 0) disk
      (= :f (nth (nth disk tail) 0)) (recur (dec tail) disk)
      :else (let [candidates (map-indexed (fn [i v] [i v]) (take (+ 1 tail) disk))
                  tail-elem (nth disk tail)
                  tail-len (nth tail-elem 2)
                  head (first (filter (fn [[_ %]] (and (= (nth % 0) :f)
                                                       (>= (nth % 2) tail-len))) candidates))]
              (if (not (some? head))
                (recur (dec tail) disk)
                (recur (dec tail) (-> disk
                                      (assoc-in [(first head)] tail-elem)
                                      (update-in [tail] (fn [elem] [:f 0 (nth elem 2)]))
                                      (insert (+ (first head) 1) (update-in (nth disk (first head)) [2] #(- % tail-len))))))))))

(defn solve [[used free]]
  (let [disk  (vec (apply concat (apply map list [(map-indexed (fn [i len] [:u i len]) used) (map-indexed (fn [_ len] [:f 0 len]) free)])))]
    [(time (checksum (part-1 disk)))
     (time (checksum (part-2 disk)))]))

(defn -main []
  (assert (= (inspect (solve (parse example))) [1928 2858]))

  (println (solve (parse (slurp "input/day9")))))
