(ns aoc-2024.day01
  (:require [aoc-2024.helpers :refer [load-lines split-by-ws str->int]]))

; https://adventofcode.com/2024/day/1

(defn lines->lists
  [lines]
  (->> lines
       (mapv split-by-ws)
       (mapv (partial mapv str->int))
       (apply mapv vector)))

(defn lists->distances
  [lists]
  (->> lists
       (mapv sort)
       (apply mapv (comp abs -))))

(defn part1
  [lines]
  (->> lines
       lines->lists
       lists->distances
       (reduce +)))

;(part1 (load-lines "day01-test.txt"))
;(part1 (load-lines "day01.txt"))

(defn lists->similarities
  [[ids tally]]
  (let [rfreq (frequencies tally)]
    (->> ids
         (mapv #(* % (get rfreq % 0))))))

(defn part2
  [lines]
  (->> lines
       lines->lists
       lists->similarities
       (reduce +)))

;(part2 (load-lines "day01-test.txt"))
;(part2 (load-lines "day01.txt"))
