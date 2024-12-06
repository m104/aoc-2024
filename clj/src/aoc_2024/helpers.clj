(ns aoc-2024.helpers
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines split]]))

(defn load-lines
  [path]
  (->> path
       io/resource
       slurp
       split-lines))

(defn split-by-ws
  [line]
  (split line #"\s+"))

(defn str->int [val] (Integer/parseInt val))

(defn invert-map
  [m]
  (reduce-kv
   (fn [m k v]
     (assoc m v k))
   {}
   m))

(defn invert-map-of-sets
  [m]
  (reduce
   (fn [m [k v]] (update m k
                         #(conj (or % #{}) v)))
   {}
   (for [[k s] m
         v s]
     [v k])))

(defn combinations
  [elements]
  (cond
    (empty? elements)      []
    (= 1 (count elements)) [[(first elements)]]
    :else                  (for [e elements
                                 combo (combinations (disj elements e))]
                             (conj combo e))))

(defn lines->grid
  [lines]
  (let [height (count lines)
        width (count (into [] (first lines)))
        data (->> lines
                  (map #(into [] %))
                  (apply concat)
                  (into []))]
    {:width width
     :height height
     :data data}))

(defn get-in-grid
  [grid x y]
  (let [{:keys [data width height]} grid]
    (when (and (< -1 x width)
               (< -1 y height))
      (let [idx (+ x (* y width))]
        (nth data idx)))))

(defn set-in-grid
  [grid x y value]
  (let [idx (+ x (* y (:width grid)))]
    (update grid :data #(assoc % idx value))))

(defn report-grid
  [grid]
  (println "** Grid:")
  (let [{:keys [width height]} grid]
    (doseq [y (range 0 height)]
      (doseq [x (range 0 width)]
        (print (str (get-in-grid grid x y) " ")))
      (println)))
  (println ""))
