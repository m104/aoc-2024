(ns aoc-2024.day03
  (:require [aoc-2024.helpers :refer [load-lines str->int]]
            [clojure.string :refer [join]]))

(defn str->mul-pairs
  [s]
  (let [pattern #"mul\((\d+),(\d+)\)"
        matches (re-seq pattern s)]
    (mapv
     (fn [[_ x y]]
       [(str->int x) (str->int y)])
     matches)))

(defn part1
  [lines]
  (->> lines
       (join "")
       str->mul-pairs
       (mapv #(apply * %))
       (reduce +)))

;(part1 (load-lines "day03-test.txt"))
;(part1 (load-lines "day03.txt"))

(defn str->instructions
  [s]
  (let [pattern #"mul\((\d+),(\d+)\)|(do)\(\)|(don't)\(\)"
        matches (re-seq pattern s)]
    (mapv
     (fn [[_ x y d dn]]
       (cond
         (and (some? x) (some? y)) ["mul" (str->int x) (str->int y)]
         d ["do"]
         dn ["don't"]))
     matches)))

(defn process
  [ops]
  (loop [result 0
         enabled true
         [[op x y] & ops] ops]
    (case op
      "mul" (recur (if enabled
                     (+ result (* x y))
                     result)
                   enabled ops)
      "do" (recur result true ops)
      "don't" (recur result false ops)
      nil result)))

(defn part2
  [lines]
  (->> lines
       (join "")
       str->instructions
       process))

;(part2 (load-lines "day03-test2.txt"))
;(part2 (load-lines "day03.txt"))
