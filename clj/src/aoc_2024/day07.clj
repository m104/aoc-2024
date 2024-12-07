(ns aoc-2024.day07
  (:require [aoc-2024.helpers :refer [load-lines split-by-ws]]
            [clojure.string :refer [replace]]
            [clojure.math.combinatorics :refer [selections]]))

(def ops-by-name
  {"mult" *
   "plus" +})

(defn line->problem
  [line]
  (let [[answer & components]
        (->> line
             split-by-ws
             (mapv (comp bigint (fn [s] (replace s ":" "")))))]
    {:answer answer :components components}))

(defn calculation
  [components operations]
  (loop [basis (first components)
         [value & components] (next components)
         [op & ops] operations]
    (cond
      (nil? op) basis
      :else (recur (op basis value) components ops))))

(defn solve-problem
  [answer components ops-by-name]
  (let [n (count components)
        operator-sets (selections (keys ops-by-name) (dec n))]
    (some (fn [ops]
            (when (= answer (calculation components (mapv #(get ops-by-name %) ops)))
              [answer (mapv str ops)]))
          operator-sets)))

(defn part1
  [lines]
  (->> lines
       (mapv line->problem)
       (mapv (fn [{:keys [answer components]}]
               (solve-problem answer components ops-by-name)))
       (filterv some?)
       (mapv first)
       (reduce +)))

;(part1 (load-lines "day07-test.txt"))
;(part1 (load-lines "day07.txt"))

(defn cat-numbers
  [x y]
  (bigint (str x y)))

(def expaneded-ops-by-name
  {"mult" *
   "plus" +
   "concat" cat-numbers})

(defn part2
  [lines]
  (->> lines
       (mapv line->problem)
       (mapv (fn [{:keys [answer components]}]
               (solve-problem answer components expaneded-ops-by-name)))
       (filterv some?)
       (mapv first)
       (reduce +)))

;(part2 (load-lines "day07-test.txt"))
;(time (part2 (load-lines "day07.txt")))