(ns aoc-2024.day11
  (:require [aoc-2024.helpers :refer [load-lines split-by-ws str->int]]
            [clojure.math :refer [log10 floor pow]]))

(defn digits [x]
  (if (= 0 x) 0 (-> x log10 floor int inc)))

(defn split-stone [stone]
  (let [len (digits stone)
        mag (int (pow 10 (/ len 2)))]
    [(quot stone mag) (rem stone mag)]))

(defn blink-stone
  [stone]
  (cond (= 0 stone) [1]
        (even? (digits stone)) (split-stone stone)
        :else [(* stone 2024)]))

(defn blink-stones
  [stones]
  (reduce
   (fn [stones stone]
     (apply conj stones (blink-stone stone)))
   []
   stones))

(defn blink-times
  [blinks stones]
  (loop [stones stones
         blinks blinks]
    (if (= 0 blinks)
      stones
      (recur (blink-stones stones) (dec blinks)))))

(defn part1
  [lines]
  (->> lines
       first
       split-by-ws
       (mapv str->int)
       (blink-times 25)
       count))

;(part1 (load-lines "day11-test.txt"))
;(part1 (load-lines "day11.txt"))

(defn fast-blink-stones
  [counts]
  (reduce
   (fn [coll [stone n]]
     (let [[s1 s2] (blink-stone stone)
           coll (update coll s1 (fn [v] (+ n (or v 0))))]
       (if s2
         (update coll s2 (fn [v] (+ n (or v 0))))
         coll)))
   {}
   counts))

(defn fast-blink-times
  [blinks counts]
  (loop [counts counts
         blinks blinks]
    (if (= 0 blinks)
      counts
      (recur (fast-blink-stones counts) (dec blinks)))))

(defn part2
  [lines]
  (->> lines
       first
       split-by-ws
       (mapv str->int)
       frequencies
       (fast-blink-times 75)
       vals
       (reduce +)))

;(part2 (load-lines "day11-test.txt"))
;(part2 (load-lines "day11.txt"))
