(ns aoc-2024.day08
  (:require [aoc-2024.helpers :refer [load-lines lines->grid]]
            [clojure.math.combinatorics :as m]))

; Antinodes
; for each freq
;   for each pair
;     symmetric deltas from pair
;       A -> B + dAB ( + dAB ...)
;       B -> A + dBA ( + dBA ...)
; count unique locations

(defn idx->coord
  [idx width]
  [(rem idx width)
   (quot idx width)])

(defn find-antenna
  [grid]
  (let [data (:data grid)
        width (:width grid)
        size (count data)
        indexed-values (mapv #(vector %1 %2) (range 0 size) data)
        antenna-by-freq (reduce
                         (fn [c [idx value]]
                           (if (= value \.)
                             c
                             (update c value
                                     #(conj (or % #{}) (idx->coord idx width)))))
                         {}
                         indexed-values)]
    (-> grid
        (assoc :antenna-by-freq antenna-by-freq))))

(defn closest-antinodes-from-coord-pair
  [[xA yA] [xB yB] _ _]
  [[(+ xA (- xA xB)) (+ yA (- yA yB))]
   [(+ xB (- xB xA)) (+ yB (- yB yA))]])

(defn find-antinodes
  [antinode-fn grid]
  (let [{:keys [antenna-by-freq width height]} grid
        antinode-groups (for [coords (vals antenna-by-freq)
                              [coordA coordB] (m/combinations coords 2)]
                          (->> (antinode-fn coordA coordB width height)
                               (filterv (fn [[x y]] (and (< -1 x width)
                                                         (< -1 y height))))))]
    (assoc grid :antinodes (into #{} (apply concat antinode-groups)))))

(defn part1
  [lines]
  (->> lines
       lines->grid
       find-antenna
       (find-antinodes closest-antinodes-from-coord-pair)
       :antinodes
       count))

;(part1 (load-lines "day08-test.txt"))
;(part1 (load-lines "day08.txt"))

(defn antinode-line
  [[xA yA] [xB yB] width height]
  (let [[dxAB dyAB] [(- xB xA) (- yB yA)]]
    (loop [[xA yA] [xA yA]
           antinodes []]
      (let [[xA yA] [(+ xA dxAB) (+ yA dyAB)]]
        (if (and (< -1 xA width) (< -1 yA height))
          (recur [xA yA] (conj antinodes [xA yA]))
          antinodes)))))

(defn all-antinodes-from-coord-pair
  [coordA coordB width height]
  (concat [coordA coordB]
          (antinode-line coordA coordB width height)
          (antinode-line coordB coordA width height)))

(defn part2
  [lines]
  (->> lines
       lines->grid
       find-antenna
       (find-antinodes all-antinodes-from-coord-pair)
       :antinodes
       count))

;(part2 (load-lines "day08-test.txt"))
;(part2 (load-lines "day08.txt"))
