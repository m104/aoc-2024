(ns aoc-2024.day05
  (:require [aoc-2024.helpers :refer [load-lines str->int]]
            [clojure.string :refer [split]]))

(defn lines->ordering-restrictions
  [lines]
  (loop [[line & lines] lines
         n 0
         restrictions {}]
    (if
     (= line "")
      {:restrictions restrictions :skip (inc n)}
      (let [[before after] (mapv str->int (split line #"\|"))]
        (recur lines (inc n)
               (update restrictions before
                       #(conj (or % #{}) after)))))))

(defn lines->page-lists
  [lines]
  (loop [[line & lines] lines
         updates []]
    (if (nil? line)
      updates
      (recur lines (->> (split line #",")
                        (mapv str->int)
                        (conj updates))))))

(defn valid-ordering?
  [restrictions pages]
  (loop [[page & pages] pages
         seen #{}]
    (cond (nil? page) true
          (some #(when (contains? seen %) %) (get restrictions page)) false
          :else (recur pages (conj seen page)))))

(defn lines->valid-updates
  [lines]
  (let [{:keys [restrictions skip]} (lines->ordering-restrictions lines)
        lines (subvec lines skip)
        page-lists (lines->page-lists lines)]
    (filterv #(valid-ordering? restrictions %) page-lists)))

(defn middle-page
  [pages]
  (nth pages (quot (count pages) 2)))

(defn part1
  [lines]
  (->> lines
       lines->valid-updates
       (mapv middle-page)
       (reduce +)))

;(part1 (load-lines "day05-test.txt"))
;(part1 (load-lines "day05.txt"))

(defn sort-page-list
  [page-list restrictions]
  (->> page-list
       (sort #(contains? (get restrictions %2) %1))
       (into [])))

(defn lines->fixed-updates
  [lines]
  (let [{:keys [restrictions skip]} (lines->ordering-restrictions lines)
        lines (subvec lines skip)
        page-lists (lines->page-lists lines)
        invalid-lists (filterv #(not (valid-ordering? restrictions %))
                               page-lists)]
    (mapv #(sort-page-list % restrictions) invalid-lists)))

(defn part2
  [lines]
  (->> lines
       lines->fixed-updates
       (mapv middle-page)
       (reduce +)))

;(part2 (load-lines "day05-test.txt"))
;(part2 (load-lines "day05.txt"))
