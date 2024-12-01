(ns aoc-2024.day01
  (:require [aoc-2024.core :refer [load-lines split-by-ws str->int]]))

; https://adventofcode.com/2024/day/1

(def test-lines (load-lines "day01-test.txt"))
(def lines (load-lines "day01.txt"))

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

(println "Part 1 test answer:")
(println (part1 test-lines))

(println "Part 1 answer:")
(println (part1 lines))

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

(println "Part 2 test answer:")
(println (part2 test-lines))

(println "Part 2 answer:")
(println (part2 lines))
