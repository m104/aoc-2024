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
  [{:keys [data width height]} x y]
  (when (and (<= 0 x (dec width))
             (<= 0 y (dec height)))
    (let [idx (+ x (* y width))]
      (nth data idx))))

(def all-directions
  ; [[dx dy] ...]
  [[0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1]])

(defn match-elements
  [grid x y [dx dy] elements]
  (loop [[element & r] elements
         x x
         y y]
    (if (nil? element)
      true
      (if (= element (get-in-grid grid x y))
        (recur r (+ x dx) (+ y dy))
        false))))

(defn search-grid
  [s directions {:keys [width height] :as grid}]
  (let [elements (into [] s)]
    (filterv some?
             (for [x (range 0 width)
                   y (range 0 height)
                   dir directions]
               (when (match-elements grid x y dir elements)
                 [x y dir])))))

(defn part1
  [lines]
  (->> lines
       lines->grid
       (search-grid "XMAS" all-directions)
       count))

(part1 (load-lines "day04-test.txt"))
(part1 (load-lines "day04.txt"))

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
       (filterv (comp #(<= 2 %) second))
       count))

(part2 (load-lines "day04-test.txt"))
(part2 (load-lines "day04.txt"))
