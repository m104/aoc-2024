(ns aoc-2024.day13
  (:require [aoc-2024.helpers :refer [load-lines str->int]]))

(defn parse-button
  [s]
  (->> (re-seq #"X\+(\d+), Y\+(\d+)" s)
       first next (mapv str->int)))

(defn parse-prize
  [s]
  (->> (re-seq #"Prize: X=(\d+), Y=(\d+)" s)
       first next (mapv str->int)))

(defn parse-machine
  [line1 line2 line3]
  {:button-a (parse-button line1)
   :button-b (parse-button line2)
   :prize (parse-prize line3)})

(defn apply-2d
  [op basis delta]
  (let [[x y] basis
        [dx dy] delta]
    [(op x dx) (op y dy)]))

(defn remaining-presses
  [target delta]
  (->> (apply-2d / target delta) (apply min) int))

(defn solve-machine
  [machine]
  ; button A = 3 tokens
  ; button B = 1 token
  (let [{:keys [button-a button-b prize]} machine]
    (loop [b-presses (remaining-presses prize button-b)
           ; [cost a-presses b-presses]
           best-combo nil]
      (if (= 0 b-presses)
        best-combo
        (let [b-action (apply-2d * button-b [b-presses b-presses])
              target (apply-2d - prize b-action)
              a-presses (max 0 (remaining-presses target button-a))
              a-action (apply-2d * button-a [a-presses a-presses])
              actual (apply-2d - target a-action)]
          (if (= actual [0 0])
            (let [cost (+ (* 3 a-presses) b-presses)]
              (if (or (nil? best-combo) (< cost (first best-combo)))
                (recur (dec b-presses) [cost a-presses b-presses])
                (recur (dec b-presses) best-combo)))
            (recur (dec b-presses) best-combo)))))))

(defn part1
  [lines]
  (->> lines
       (partition-all 4)
       (mapv #(take 3 %))
       (mapv #(apply parse-machine %))
       (mapv solve-machine)
       (filterv some?)
       (mapv first)
       (reduce +)))

;(part1 (load-lines "day13-test.txt"))
;(part1 (load-lines "day13.txt"))

; ref: https://en.wikipedia.org/wiki/Cramer%27s_rule#Explicit_formulas_for_small_systems
(defn fast-solve-machine
  [machine]
  (let [{:keys [button-a button-b prize]} machine
        [px py] prize
        [ax ay] button-a
        [bx by] button-b
        a-presses (/ (- (* px by) (* py bx))
                     (- (* ax by) (* ay bx)))
        b-presses (/ (- px (* ax a-presses))
                     bx)]
    ; looking for an exact solution only
    (when (and (not (ratio? a-presses))
               (not (ratio? b-presses)))
      [(+ (* 3 a-presses) b-presses) a-presses b-presses])))

(defn fix-machine
  [machine factor]
  (assoc machine :prize
         (apply-2d + (:prize machine) [factor factor])))

(defn part2
  [lines]
  (->> lines
       (partition-all 4)
       (mapv #(take 3 %))
       (mapv #(apply parse-machine %))
       (mapv #(fix-machine % 10000000000000))
       (mapv fast-solve-machine)
       (filterv some?)
       (mapv first)
       (reduce +)))

;(part2 (load-lines "day13-test.txt"))
;(part2 (load-lines "day13.txt"))