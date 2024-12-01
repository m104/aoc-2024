(ns aoc-2024.day01
  (:require [aoc-2024.core :refer [load-lines split-by-ws str->int]]))

; https://adventofcode.com/2024/day/1

(def test-lines (load-lines "day01-test.txt"))
(def lines (load-lines "day01.txt"))

(defn lines->lists
  [lines]
  (reduce
   (fn [[left right] line]
     (let [[l r] (-> line
                     split-by-ws
                     ((partial mapv str->int)))]
       [(conj left l) (conj right r)]))
   [[] []]
   lines))

(defn lists->distances
  [left right]
  (loop [[l & left] (sort left)
         [r & right] (sort right)
         distances []]
    (if (nil? l)
      distances
      (recur left right
             (conj distances (abs (- r l)))))))

(defn part1
  [lines]
  (let [[left right] (lines->lists lines)
        distances (lists->distances left right)]
    (reduce + distances)))

(println "Part 1 test answer:")
(println (part1 test-lines))

(println "Part 1 answer:")
(println (part1 lines))


(defn part2
  [lines]
  (let [[ids right] (lines->lists lines)
        rfreq (frequencies right)]
    (reduce
     (fn [similarity id]
       (+ similarity
          (* id (get rfreq id 0))))
     0
     ids)))

(println "Part 2 test answer:")
(println (part2 test-lines))

(println "Part 2 answer:")
(println (part2 lines))
