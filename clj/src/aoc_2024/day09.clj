(ns aoc-2024.day09
  (:require [aoc-2024.helpers :refer [load-lines str->int]]
            [clojure.string :as s]
            [medley.core :refer [indexed]]))

(defn read-filetable
  "[[block-start size file-id?] ...]"
  [input]
  (loop [[[file-id [size free]] & input] (indexed (partition-all 2 input))
         filetable []
         block 0]
    (if (nil? file-id)
      filetable
      (recur input
             (-> filetable
                 (conj [block size file-id])
                 (conj [(+ block size) (or free 0) nil]))
             (+ block size (or free 0))))))

(defn expand-blocks
  [filetable]
  (->> filetable
       (mapv
        (fn [[_ blocks file-id]]
          (if file-id
            (repeat blocks file-id)
            (repeat blocks nil))))
       (apply concat)
       (into [])))

(defn defrag-blocks
  [blocks]
  (let [size (count blocks)
        free-blocks (count (filterv nil? blocks))
        used-blocks (- size free-blocks)
        fwd (take used-blocks blocks)
        rev (take free-blocks (reverse blocks))]
    (loop [[block & blocks :as fwd] fwd
           [rblock & rblocks :as rev] rev
           defragged []]
      (cond (= used-blocks (count defragged)) defragged
            block (recur blocks rev (conj defragged block))
            rblock (recur blocks rblocks (conj defragged rblock))
            :else (recur fwd rblocks defragged)))))

(defn checksum
  [filetable]
  (->> filetable
       indexed
       (mapv (fn [[idx file-id]] (* idx (or file-id 0))))
       (reduce +)))

(defn part1
  [lines]
  (let [coded (mapv str->int (-> lines first (s/split #"")))]
    (-> coded
        read-filetable
        expand-blocks
        defrag-blocks
        checksum)))

;(part1 (load-lines "day09-test.txt"))
;(part1 (load-lines "day09.txt"))

(defn defrag-files
  [filetable]
  (loop [[file & files] (reverse (filterv #(last %) filetable))
         free-entries (filterv #(nil? (last %)) filetable)
         defragged []]
    (if (nil? file) (sort-by #(first %) (concat defragged free-entries))
        (let [[block size id] file
              free-entry (some #(when (>= (second %) size) %)
                               (take-while #(>= block (first %)) free-entries))]
          (if (nil? free-entry)
            (recur files free-entries (conj defragged [block size id]))
            (let [[free-block free-size _] free-entry
                  free-index (.indexOf free-entries free-entry)
                  free-entries (-> free-entries
                                   (assoc free-index
                                          [(+ free-block size) (- free-size size) nil])
                                   (conj [block size nil]))]
              (recur files free-entries (conj defragged [free-block size id]))))))))

(defn part2
  [lines]
  (let [coded (mapv str->int (-> lines first (s/split #"")))]
    (-> coded
        read-filetable
        defrag-files
        expand-blocks
        checksum)))

;(part2 (load-lines "day09-test.txt"))
;(part2 (load-lines "day09.txt"))
