(ns aoc-2024.day10
  (:require [aoc-2024.helpers :refer [load-lines lines->grid str->int idx->coord get-in-grid]]
            [clojure.set :refer [union]]))

(def directions
  [[1 0] [0 1] [-1 0] [0 -1]])

(defn find-trailheads
  [grid]
  (let [{:keys [data width height]} grid
        size (* width height)
        indexed (mapv #(vector %1 %2) (range 0 size) data)]
    (->> indexed
         (filterv (fn [[_ value]] (= 0 value)))
         (mapv (comp (partial idx->coord width) first)))))

(defn walk-trailheads
  [grid level start]
  (let [{:keys [width height]} grid
        [x y] start
        looking-for (inc level)
        adjacents (->> directions
                       (mapv (fn [[dx dy]] [(+ x dx) (+ y dy)]))
                       (filterv (fn [[x y]] (and (< -1 x width)
                                                 (< -1 y height)))))
        inclines (filterv (fn [[x y]] (= looking-for (get-in-grid grid x y)))
                          adjacents)]
    (cond (empty? inclines) #{}
          (= looking-for 9) (into #{} inclines)
          :else (->> inclines
                     (mapv #(walk-trailheads grid looking-for %))
                     (reduce union #{})))))

(defn part1
  [lines]
  (let [grid (lines->grid lines)
        subst (fn [v] (if (= v ".") "-1" v))
        grid (update grid :data #(mapv (comp str->int subst str) %))
        trailheads (find-trailheads grid)]
    (->> trailheads
         (mapv (partial walk-trailheads grid 0))
         (mapv count)
         (reduce +))))

;(part1 (load-lines "day10-test.txt"))
;(part1 (load-lines "day10.txt"))

(defn count-all-trails
  [grid level start]
  (let [{:keys [width height]} grid
        [x y] start
        looking-for (inc level)
        adjacents (->> directions
                       (mapv (fn [[dx dy]] [(+ x dx) (+ y dy)]))
                       (filterv (fn [[x y]] (and (< -1 x width)
                                                 (< -1 y height)))))
        inclines (filterv (fn [[x y]] (= looking-for (get-in-grid grid x y)))
                          adjacents)]
    (cond (empty? inclines) 0
          (= looking-for 9) (count inclines)
          :else (->> inclines
                     (mapv #(count-all-trails grid looking-for %))
                     (reduce +)))))

(defn part2
  [lines]
  (let [grid (lines->grid lines)
        subst (fn [v] (if (= v ".") "-1" v))
        grid (update grid :data #(mapv (comp str->int subst str) %))
        trailheads (find-trailheads grid)]
    (->> trailheads
         (mapv (partial count-all-trails grid 0))
         (reduce +))))

;(part2 (load-lines "day10-test.txt"))
;(part2 (load-lines "day10.txt"))
