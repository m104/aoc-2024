(ns aoc-2024.day12
  (:require [aoc-2024.helpers :refer [load-lines lines->grid idx->coord report-grid]]
            [medley.core :refer [map-vals]]
            [clojure.set :refer [intersection difference union]]))

(def directions
  [[0 -1] [1 0] [0 1] [-1 0]])

(defn find-neighbors
  [[x y]]
  (->> directions
       (mapv (fn [[dx dy]] [(+ x dx) (+ y dy)]))))

(defn find-regions
  [coords]
  (loop [[coord & coords] coords
         regions []]
    (if (nil? coord)
      regions
      (let [neighbors (set (find-neighbors coord))
            flagged (group-by
                     (fn [region] (->> neighbors
                                       (intersection region)
                                       seq
                                       boolean))
                     regions)
            found-region (apply union #{coord} (get flagged true))
            other-regions (or (get flagged false) [])]
        (recur coords
               (into [] (conj other-regions found-region)))))))

(defn find-plant-regions
  [grid]
  (let [{:keys [data width height]} grid
        indexed (mapv #(vector %1 %2) (range 0 (* width height)) data)
        coords-by-plant (reduce (fn [coll [idx plant]]
                                  (update coll plant #(conj (or % #{}) (idx->coord width idx))))
                                {} indexed)]
    (reduce
     (fn [coll [plant coords]]
       (assoc coll plant (find-regions coords)))
     {} coords-by-plant)))

(defn price-region
  [region]
  (let [area (count region)
        perimeter (->> region
                       (mapv (fn [coord] (count (difference (set (find-neighbors coord))
                                                            region))))
                       (reduce +))]
    (* area perimeter)))

(defn part1
  [lines]
  (let [grid (lines->grid lines)
        regions-by-plant (find-plant-regions grid)
        price-by-plant (map-vals (fn [regions]
                                   (reduce + (mapv price-region regions)))
                                 regions-by-plant)]
    (->> price-by-plant
         vals
         (reduce +))))

;(part1 (load-lines "day12-test.txt"))
;(part1 (load-lines "day12.txt"))

(defn count-faces
  [region]
  (loop [[[dx dy] & dirs] directions
         faces 0]
    (if (nil? dx) faces
        (let [directed-neighbors (difference
                                  (set (mapv (fn [[x y]] [(+ x dx) (+ y dy)])
                                             region))
                                  region)
              regions (find-regions directed-neighbors)]
          (recur dirs (+ faces (count regions)))))))

(defn bulk-price-region
  [region]
  (let [area (count region)
        sides (count-faces region)]
    (* area sides)))

(defn part2
  [lines]
  (let [grid (lines->grid lines)
        regions-by-plant (find-plant-regions grid)
        price-by-plant (map-vals (fn [regions]
                                   (reduce + (mapv bulk-price-region regions)))
                                 regions-by-plant)]

    (->> price-by-plant
         vals
         (reduce +))))

;(part2 (load-lines "day12-test.txt"))
;(part2 (load-lines "day12.txt"))
