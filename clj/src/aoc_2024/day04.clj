(ns aoc-2024.day04
  (:require [aoc-2024.helpers :refer [load-lines lines->grid get-in-grid]]))

(def all-directions
  ; [[dx dy] ...]
  [[0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1]])

(defn match-chars?
  [grid x y [dx dy] chars]
  (loop [[element & r] chars
         x x
         y y]
    (if (nil? element)
      true
      (if (= element (get-in-grid grid x y))
        (recur r (+ x dx) (+ y dy))
        false))))

(defn direction-search
  [grid x y directions chars]
  (when (= (first chars) (get-in-grid grid x y))
    (reduce
     (fn [matches dir]
       (if (match-chars? grid x y dir chars)
         (conj matches [x y dir])
         matches))
     []
     directions)))

(defn search-grid
  "Search the grid for the string, in the given directions.

   Returns a vector of [x y [dx dy]] values corresponding to the
   start of each match string and the direction of the match."
  [string directions {:keys [width height] :as grid}]
  (let [chars (into [] string)]
    (->> (for [x (range 0 width)
               y (range 0 height)]
           (direction-search grid x y directions chars))
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
       ; increment each match by one directional grid position
       ; e.g. starting from "M", move to "A" position
       (mapv (fn [[x y [dx dy]]]
               [(+ x dx) (+ y dy)]))
       ; gather up matching "A" grid positions
       frequencies
       ; only count "A" positions that have a match
       (filterv (comp #(= 2 %) second))
       count))

;(part2 (load-lines "day04-test.txt"))
;(part2 (load-lines "day04.txt"))
