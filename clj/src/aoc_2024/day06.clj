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

(defn try-guard-walk
  [grid guard-coords dir]
  (let [{:keys [width height] :as grid} grid
        [x y] guard-coords
        [dx dy] dir
        next-x (+ x dx)
        next-y (+ y dy)]
    (cond
      ; out of the frame
      (not (and (< -1 next-x width)
                (< -1 next-y height)))
      {:grid (set-in-grid grid x y \X) :outcome :outside}
      ; hit an obstacle
      (= \# (get-in-grid grid next-x next-y))
      {:grid grid :guard-coords [x y] :outcome :obstacle}
      ; successfully moved
      :else
      {:grid (-> grid
                 (set-in-grid x y \X)
                 (set-in-grid next-x next-y \^))
       :guard-coords [next-x next-y]
       :outcome :forward})))

(defn guard-walk
  [grid]
  (loop [grid grid
         guard-coords (get-starting-pos grid)
         dirs (cycle directions)
         turns #{}
         iter 0]
    (let [dir (first dirs)
          {:keys [grid guard-coords outcome]} (try-guard-walk grid guard-coords dir)]
      (cond
        ; be reasonable
        (= 10000 iter)
        {:grid grid :outcome :safety}
        ; cycle detection
        (contains? turns [guard-coords dir])
        {:grid grid :outcome :loop}
        ; outside of the grid
        (= outcome :outside)
        {:grid grid :outcome :outside}
        ; hit an obstacle
        (= outcome :obstacle)
        (recur grid guard-coords (next dirs) (conj turns [guard-coords dir]) (inc iter))
        ; walking forward
        :else
        (recur grid guard-coords dirs turns (inc iter))))))

(defn part1
  [lines]
  (->> lines
       lines->grid
       guard-walk
       :grid
       :data
       (filterv #(= % \X))
       count))

;(part1 (load-lines "day06-test.txt"))
;(part1 (load-lines "day06.txt"))

(defn find-walked
  [grid]
  (let [{:keys [width height]} grid
        coords (for [y (range 0 height)
                     x (range 0 width)]
                 [x y])]
    (filterv
     #(= \X (get-in-grid grid (first %) (second %)))
     coords)))

(defn part2
  [lines]
  (let [grid (lines->grid lines)
        guard-coords (get-starting-pos grid)
        walked (guard-walk grid)
        walked-grid (:grid walked)
        walked-coordinates (->> walked-grid
                                find-walked
                                (filterv #(not (= % guard-coords))))]
    (count (filterv
            (fn [[x y]]
              (-> grid
                  (set-in-grid x y \#)
                  guard-walk
                  :outcome
                  (= :loop)))
            walked-coordinates))))

;(part2 (load-lines "day06-test.txt"))
;(part2 (load-lines "day06.txt"))
