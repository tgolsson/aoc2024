(ns aoc2024.day18
  (:require [aoc2024.util :refer :all]
            [clojure.string]
            [clojure.data.priority-map :refer [priority-map-keyfn]]
            [clojure.java.io]))

(def example (multi-line
              "5,4" "4,2" "4,5" "3,0" "2,1"
              "6,3" "2,4" "1,5" "0,6" "3,3"
              "2,6" "5,1" "1,2" "5,5" "2,5"
              "6,5" "1,4" "0,4" "6,4" "1,1"
              "6,1" "1,0" "0,5" "1,6" "2,0"))

(defn parse-pair [pair]
  (mapv Integer/parseInt  (clojure.string/split pair  #",")))

(defn parse [rdr]
  (mapv parse-pair (line-seq rdr)))

(defn apply-byte [grid byte]
  (set-at grid byte \#))

(defrecord Node [position g h previous])

(defn expand-children [grid node end expanded valid?]
  (->> (neighbours-with-value grid (:position node) \. valid?)
       (filter #(or (not (contains? expanded %))
                    (< (inc (:g node)) (get expanded %))))
       (map #(Node. % (inc (:g node)) (manhattan % end)
                    (conj (:previous node) (:position node))))))

(defn astar [grid start end valid?]
  (loop [node     (Node. start 0 (manhattan start end) [])
         nodes    (priority-map-keyfn (fn [node] (+ (:g node) (:h node))))
         expanded {}]
    (if (= (:position node) end)
      node
      (let [children (expand-children grid node end expanded valid?)
            all-nodes (into nodes (map (fn [child] [child child]) children))]
        (if (seq all-nodes)
          (recur (second (peek all-nodes)) (pop all-nodes) (assoc expanded (:position node) (:g node)))
          nil)))))

(defn binsearch [grid bytes w h valid?]
  (loop [start 1
         end (count bytes)]
    (if (>= start (dec end))
      start
      (let [mid (+ start (quot (- end start) 2))
            res (astar (reduce apply-byte grid (take mid bytes)) [0 0] [(dec w) (dec h)] valid?)]

        (if res
          (recur mid end)
          (recur start mid))))))

(defn solve [rdr w h taken]
  (let [bytes (parse rdr)
        grid (mapv vec (repeat h (repeat w \.)))
        valid? (inside-grid? grid)]
    [(:g (astar (reduce apply-byte grid (take taken bytes)) [0 0] [(dec w) (dec h)] valid?))
     (nth bytes (binsearch grid bytes w h valid?))]))

(defn -main []
  (assert (= (inspect (solve (string-reader example) 7 7 12)) [22 [6 1]]))
  (with-open [rdr (clojure.java.io/reader "input/day18")]
    (println (solve rdr 71 71 1024))))
