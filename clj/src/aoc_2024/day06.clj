(ns aoc-2024.day06
  (:require [aoc-2024.helpers :refer [load-lines lines->grid get-in-grid set-in-grid report-grid]]))

(def directions
  ; [dx dy]
  [[0 -1] [1 0] [0 1] [-1 0]])

(defn get-starting-pos
  [{:keys [data width height]}]
  (let [idx (.indexOf data \^)
        x (rem idx width)
        y (quot idx height)]
    [x y]))

(defn walk-guard-forward
  [grid guard-coords dir]
  (loop [{:keys [width height] :as grid} grid
         [x y] guard-coords]
    (let [[dx dy] dir
          next-x (+ x dx)
          next-y (+ y dy)]
      (cond
        ; out of the frame
        (not (and (< -1 next-x width)
                  (< -1 next-y height)))
        {:grid (set-in-grid grid x y \X) :guard-coords nil}
        ; hit a wall
        (= \# (get-in-grid grid next-x next-y))
        {:grid grid :guard-coords [x y]}
        ; successfully moved
        :else
        (recur (-> grid
                   (set-in-grid x y \X)
                   (set-in-grid next-x next-y \^))
               [next-x next-y])))))

(defn walk-guard
  [grid]
  (loop [grid grid
         guard-coords (get-starting-pos grid)
         dirs (cycle directions)]
    (let [[dir & dirs] dirs
          {:keys [grid guard-coords]} (walk-guard-forward grid guard-coords dir)]
      (if guard-coords
        (recur grid guard-coords dirs)
        grid))))

(defn part1
  [lines]
  (->> lines
       lines->grid
       walk-guard
       :data
       (filterv #(= % \X))
       count))

(part1 (load-lines "day06-test.txt"))
(part1 (load-lines "day06.txt"))

(defn part2
  [lines]
  (->> lines
       count))

;(part2 (load-lines "day06-test.txt"))
;(part2 (load-lines "day06.txt"))
