(ns aoc-2024.day16
  (:require [aoc-2024.helpers :refer [load-lines lines->grid report-grid apply-2d
                                      find-grid-locations get-in-grid set-in-grid]]
            [clojure.set :refer [union difference intersection]]))

(def directions
  [[0 -1] [1 0] [0 1] [-1 0]])

(defn draw-grid [walls start end]
  (let [width (inc (apply max (mapv first walls)))
        height (inc (apply max (mapv second walls)))]
    (println "Grid:")
    (doseq [y (range 0 height)]
      (doseq [x (range 0 width)]
        (let [coord [x y]
              value (cond (= start coord) "S"
                          (= end coord) "E"
                          (contains? walls coord) "#"
                          :else ".")]
          (print value)))
      (println))
    (println)))

(defn next-coords
  [walls coord]
  (->> directions
       (mapv (fn [dir] (apply-2d + coord dir)))
       (filterv (fn [coord] (not (contains? walls coord))))))

(defn find-paths
  [walls visited start end]
  (if (= start end)
    [(conj visited end)]
    (let [unavailable (union walls (into #{} visited) #{start})
          available (next-coords unavailable start)
          new-visited (conj visited start)]
      ;(println "start" start "available" available)
      (when (seq available)
        (let [paths (apply concat (mapv #(find-paths walls new-visited % end) available))]
          (reduce
           (fn [all-paths path]
             (conj all-paths path))
           []
           paths))))))

(defn path-directions
  [coords]
  (loop [start (first coords)
         [coord & coords] (next coords)
         path []]
    (if (nil? coord) path
        (recur coord coords (conj path (apply-2d - start coord))))))

(defn score-path
  [path]
  (let [directions (path-directions path)
        partitioned (partition-by identity directions)]
    (+ (* 1000 (count partitioned))
       (reduce + (mapv count partitioned)))))

(defn part1
  [lines]
  (let [grid (lines->grid lines)
        locations (find-grid-locations grid #{\# \S \E})
        start (first (get locations \S))
        end (first (get locations \E))
        walls (into #{} (get locations \#))
        paths (find-paths walls [] start end)
        scored-paths (mapv score-path paths)]
    (println "Paths" (count paths))
    (println "Scores" (sort scored-paths))
    (draw-grid walls start end)
    (apply min scored-paths)))

(part1 (load-lines "day16-test.txt"))
(part1 (load-lines "day16-test2.txt"))
(part1 (load-lines "day16.txt"))

(defn part2
  [lines]
  (->> lines
       count))

;(part2 (load-lines "day16-test.txt"))
;(part2 (load-lines "day16.txt"))
