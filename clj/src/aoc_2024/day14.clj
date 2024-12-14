(ns aoc-2024.day14
  (:require [aoc-2024.helpers :refer [load-lines str->int apply-2d
                                      report-grid get-in-grid set-in-grid]]))

(defn parse-xy-dxdy
  [s]
  ; p=x,y v=dx,dy
  (->> (re-seq #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)" s)
       first next (mapv str->int)))

(defn simulate
  [steps [x y dx dy]]
  (->> [dx dy]
       (apply-2d * [steps steps])
       (apply-2d + [x y])))

(defn constrain
  [width height [x y]]
  [(mod x width) (mod y height)])

(defn count-in-quadrants
  [width height coords]
  (let [halfway-x (/ (if (odd? width) (dec width) width) 2)
        halfway-y (/ (if (odd? height) (dec height) height) 2)]
    (reduce
     (fn [quadrant-counts [x y]]
       (let [quadrant (cond (and (< x halfway-x) (< y halfway-y)) 0
                            (and (>= x (- width halfway-x)) (< y halfway-y)) 1
                            (and (>= x (- width halfway-x)) (>= y (- height halfway-y))) 2
                            (and (< x halfway-x) (>= y (- height halfway-y))) 3
                            :else nil)]
         (if quadrant
           (update quadrant-counts quadrant #(inc (or % 0)))
           quadrant-counts)))
     {}
     coords)))

(defn part1
  [steps width height lines]
  (->> lines
       (mapv parse-xy-dxdy)
       (mapv (partial simulate steps))
       (mapv (partial constrain width height))
       (count-in-quadrants width height)
       vals
       (reduce *)))

;(part1 100 11 7 (load-lines "day14-test.txt"))
;(part1 100 101 103 (load-lines "day14.txt"))

(defn draw-robots
  [coords grid]
  (loop [grid grid
         [[x y] & coords] coords]
    (if (nil? x)
      (report-grid grid)
      (let [value (get-in-grid grid x y)
            new-value (if (int? value) (inc value) 1)]
        (recur (set-in-grid grid x y new-value) coords)))))

(defn part2
  [max-steps width height lines]
  (let [input (->> lines (mapv parse-xy-dxdy))]
    (loop [step 0
           [best-score best-step] nil]
      (if (= max-steps step)
        best-step
        (let [coords (->> input (mapv (partial simulate step))
                          (mapv (partial constrain width height)))
              score (->> coords
                         frequencies
                         vals
                         (apply max))
              [best-score best-step] (if (or (nil? best-score) (< score best-score))
                                       [score step]
                                       [best-score best-step])]
          (recur (inc step) [best-score best-step]))))))

;(part2 10000 101 103 (load-lines "day14.txt"))
