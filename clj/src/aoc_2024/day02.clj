(ns aoc-2024.day02
  (:require [aoc-2024.helpers :refer [load-lines split-by-ws str->int]]))

; https://adventofcode.com/2024/day/2

(defn lines->reports
  [lines]
  (->> lines
       (mapv split-by-ws)
       (mapv (partial mapv str->int))))

(defn safe-report?
  [levels]
  (let [[level0 & rest] levels
        level1 (first rest)
        direction (if (pos? (- level1 level0)) 1 -1)]
    (loop [basis level0
           [level & rest] rest]
      (let [delta (- level basis)
            dir (if (pos? delta) 1 -1)
            delta (abs delta)]
        (cond (not= direction dir) false
              (not (<= 1 delta 3)) false
              (nil? rest) true
              :else (recur level rest))))))

(defn part1
  [lines]
  (->> lines
       lines->reports
       (filterv safe-report?)
       count))

;(part1 (load-lines "day02-test.txt"))
;(part1 (load-lines "day02.txt"))

(defn elided-set
  [levels]
  (let [len (count levels)]
    (mapv
     #(into [] (concat (subvec levels 0 %) (subvec levels (inc %))))
     (range 0 len))))

(defn levels->skips
  [levels]
  (loop [basis (first levels)
         [level & levels] (rest levels)
         skips []]
    (if (nil? level) skips
        (recur level levels
               (conj skips (- level basis))))))

(defn first-invalid-skip
  [skips]
  (let [skip (first skips)
        direction (if (pos? skip) pos? neg?)]
    (loop [[skip & rem] skips
           n 0]
      (cond (nil? skip) nil
            (not (direction skip)) n
            (not (<= 1 (abs skip) 3)) n
            :else (recur rem (inc n))))))

(defn valid-skips?
  [skips]
  (nil? (first-invalid-skip skips)))

(defn almost-valid-report
  [levels]
  (some #(when (valid-skips? (levels->skips %)) %)
        (concat [levels] (elided-set levels))))

(defn part2
  [lines]
  (->> lines
       lines->reports
       (filterv almost-valid-report)
       count))

;(part2 (load-lines "day02-test.txt"))
;(part2 (load-lines "day02.txt"))
