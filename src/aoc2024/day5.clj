(ns aoc2024.day5
  (:require [aoc2024.util :refer :all]
            [clojure.string]
            [clojure.java.io]))

(def example
  (multi-line "47|53"
              "97|13"
              "97|61"
              "97|47"
              "75|29"
              "61|13"
              "75|53"
              "29|13"
              "97|29"
              "53|29"
              "61|53"
              "97|53"
              "61|29"
              "47|13"
              "75|47"
              "97|75"
              "47|61"
              "75|61"
              "47|29"
              "75|13"
              "53|13"
              ""
              "75,47,61,53,29"
              "97,61,53,29,13"
              "75,29,13"
              "75,97,47,61,53"
              "61,13,29"
              "97,13,75,29,47"))

(defrecord Constraint
           [page must-before])

(defn split-parse [sep content]
  (mapv Integer/parseInt (clojure.string/split content (re-pattern sep))))

(defn parse [rdr]
  (let [lines (line-seq rdr)
        [first second] (split-with #(not= % "") lines)]
    [(->> first
          (map #(split-parse "\\|" %))
          (mapv (fn [[p mb]] (Constraint. p mb))))

     (->> (rest second)
          (mapv #(split-parse "," %)))]))

(defn validate-constraint [constraints page previous]
  (every? (fn [constraint]  (not (some #(= % (:must-before constraint)) previous)))
          (find-all #(= (:page %) page) constraints)))

(defn validate-constraint-future [constraints page remainder]
  (->> remainder
       (map (fn [candidate] (find-all #(= (:page %) candidate) constraints)))
       flatten
       (not-any? #(= page (:must-before %)))))

(defn validate-constraints [constraints order]
  (loop [pre []
         current (first order)
         tail (rest order)]
    (cond
      (not (some? current)) true
      (and (validate-constraint constraints current pre) (validate-constraint-future constraints current tail))
      (recur (conj pre current) (first tail) (rest tail))
      :else false)))

(defn reorder [constraints prefix remainder]
  (if (empty? remainder)
    prefix
    (->> remainder
         (map-indexed (fn [a b] [a b]))
         (filter (fn [[i %]] (and (validate-constraint constraints % prefix)
                                  (validate-constraint-future constraints % (vec-remove i remainder)))))
         (map (fn [[i page]] (reorder constraints (conj prefix page) (vec-remove i remainder))))
         (filter some?)
         first)))

(defn solve [[constraints orders]]
  (let [validity (group-by #(validate-constraints constraints %) orders)]
    (mapv #(->> % (mapv middle) sum)
          [(get validity true)
           (map #(reorder constraints [] %) (get validity false))])))

(defn -main []
  (assert (= (inspect (solve (parse (string-reader example)))) [143 123]))

  (with-open [rdr (clojure.java.io/reader "input/day5")]
    (println (solve (parse rdr)))))
