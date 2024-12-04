(ns aoc-2024.day04
  (:require [aoc-2024.helpers :refer [load-lines]]))

(defn lines->grid
  [lines]
  (let [height (count lines)
        width (count (into [] (first lines)))
        data (->> lines
                  (map #(into [] %))
                  (apply concat)
                  (into []))]
    {:width width
     :height height
     :data data}))

(defn get-in-grid
  [x y {:keys [data width height]}]
  (when (and (<= 0 x (dec width))
             (<= 0 y (dec height)))
    (let [idx (+ x (* y width))]
      (nth data idx))))

(def all-directions
  ; [[dx dy] ...]
  [[0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1]])

(defn match-chars?
  [x y [dx dy] chars grid]
  (loop [[element & r] chars
         x x
         y y]
    (if (nil? element)
      true
      (if (= element (get-in-grid x y grid))
        (recur r (+ x dx) (+ y dy))
        false))))

(defn direction-search
  [x y directions chars grid]
  (when (= (first chars) (get-in-grid x y grid))
    (reduce
     (fn [matches dir]
       (if (match-chars? x y dir chars grid)
         (conj matches [x y dir])
         matches))
     []
     directions)))

(defn search-grid
  [string directions {:keys [width height] :as grid}]
  (let [chars (into [] string)]
    (->> (for [x (range 0 width)
               y (range 0 height)]
           (direction-search x y directions chars grid))
         (filterv some?)
         (apply concat)
         (into []))))

(defn part1
  [lines]
  (->> lines
       lines->grid
       (search-grid "XMAS" all-directions)
       count))

;(part1 (load-lines "day04-test.txt"))
;(part1 (load-lines "day04.txt"))

(def x-directions
  ; [[dx dy] ...]
  [[1 -1] [1 1] [-1 1] [-1 -1]])

(defn part2
  [lines]
  (->> lines
       lines->grid
       (search-grid "MAS" x-directions)
       (mapv (fn [[x y [dx dy]]]
               [(+ x dx) (+ y dy)]))
       frequencies
       (into [])
       (filterv (comp #(= 2 %) second))
       count))

;(part2 (load-lines "day04-test.txt"))
;(part2 (load-lines "day04.txt"))
